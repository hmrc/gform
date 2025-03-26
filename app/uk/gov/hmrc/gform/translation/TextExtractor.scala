/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.translation

import cats.implicits._
import com.github.tototoshi.csv.{ CSVReader, CSVWriter }
import io.circe._
import io.circe.syntax._
import io.circe.CursorOp._
import io.circe.parser._
import java.io.{ BufferedOutputStream, BufferedReader, StringReader }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Constant

case class Lang(en: String, cy: String)

object Lang {

  implicit val langDecoder: Decoder[Lang] = c => {
    val enCy = for {
      en <- c.get[String]("en")
      cy <- c.get[String]("cy")
    } yield Lang(en, cy)
    val enOnly = for {
      en <- c.get[String]("en")
    } yield Lang(en, "")

    enCy.orElse(enOnly)
  }
}

case class TranslatedRow(
  en: String,
  cy: String
)

case class EnTextToTranslate(
  en: String
)

case class Row(
  path: String,
  en: String,
  cy: String
)

case class MissingRow(
  path: String,
  en: String
)

sealed trait ReductionInProgress extends Product with Serializable

object ReductionInProgress {
  case class Wrapper(instruction: Instruction) extends ReductionInProgress
  case class DeepTraverse(depth: Int) extends ReductionInProgress
}

sealed trait Instruction extends Product with Serializable

object Instruction {
  case class Pure(op: CursorOp) extends Instruction
  case object TraverseArray extends Instruction
}

object Translator {
  def apply(json: Json, paths: List[List[Instruction]]) = {
    val topLevelExprData = TopLevelExprData.from(json)
    new Translator(json, paths, topLevelExprData)
  }

  import ReductionInProgress._
  import Instruction._

  def resolveInstruction(instructions: List[Instruction], cursor: HCursor): List[ReductionInProgress] = {
    val (_, reducingInProgress, _) =
      instructions.foldRight[(ACursor, List[ReductionInProgress], Boolean)](
        (cursor, List.empty[ReductionInProgress], false)
      ) { case (instruction, (aCursor, reducingInProgress, deepTraverseIsDefined)) =>
        instruction match {
          case Pure(cursorOp) =>
            (aCursor.replayOne(cursorOp), Wrapper(instruction) :: reducingInProgress, deepTraverseIsDefined)
          case TraverseArray if !deepTraverseIsDefined =>
            val maybeArray: Option[Json] = aCursor.focus
            val arraySize: Option[Int] = maybeArray.flatMap(_.asArray).map(_.size)
            arraySize.fold((aCursor, reducingInProgress, deepTraverseIsDefined)) { size =>
              (aCursor, DeepTraverse(size) :: Wrapper(Pure(DownArray)) :: reducingInProgress, true)
            }
          case TraverseArray =>
            (aCursor, Wrapper(instruction) :: reducingInProgress, deepTraverseIsDefined)
        }
      }

    reducingInProgress
  }

  def eliminateDeepTraverse(steps: List[ReductionInProgress]): List[List[Instruction]] = {
    val (nextSteps, instructions) = steps.foldRight((List.empty[ReductionInProgress], List.empty[Instruction])) {
      case (step, (forReduction, instructions)) =>
        step match {
          case Wrapper(instruction) => (step :: forReduction, instruction :: instructions)
          case DeepTraverse(arraySize) if arraySize > 1 =>
            (DeepTraverse(arraySize - 1) :: Wrapper(Pure(MoveRight)) :: forReduction, instructions)
          case DeepTraverse(arraySize) => (forReduction, instructions)
        }
    }

    val unresolved = nextSteps.exists {
      case DeepTraverse(_) => true
      case _               => false
    }

    instructions :: (if (unresolved) eliminateDeepTraverse(nextSteps) else Nil)
  }

  def generateOpHistory(instructions: List[Instruction]): List[CursorOp] =
    instructions.foldRight(List.empty[CursorOp]) { case (step, cursorOpHistory) =>
      step match {
        case Pure(op)   => op :: cursorOpHistory
        case unexpected => throw new Exception(s"Cannot convert $unexpected to CursorOp")
      }
    }
}

class Translator(json: Json, paths: List[List[Instruction]], val topLevelExprData: TopLevelExprData) {

  import Translator._
  import Instruction._

  private val smartStringCursors: List[HCursor => ACursor] = {
    def loop(instructions: List[Instruction], cursor: HCursor): List[List[Instruction]] = {
      val reducingInProgress: List[ReductionInProgress] = resolveInstruction(instructions, cursor)
      val instructionsList: List[List[Instruction]] = eliminateDeepTraverse(reducingInProgress)

      instructionsList.flatMap { instructions =>
        val needTraversingDeeper = instructions.exists {
          case TraverseArray => true
          case _             => false
        }
        if (needTraversingDeeper) loop(instructions, cursor) else List(instructions)
      }
    }

    val smartStringPaths = paths.map(Instruction.TraverseArray :: _) ++ paths
    val topCursor: HCursor = json.hcursor
    val cursorOps: List[List[CursorOp]] =
      smartStringPaths.flatMap { path =>
        loop(path, topCursor).map(generateOpHistory)
      }

    val cursors: List[HCursor => ACursor] = replyCursorOps(cursorOps)
    cursors
  }

  private def applyOperations[A](
    smartStringOperation: (HCursor, List[HCursor => ACursor]) => HCursor,
    topLevelOperation: (HCursor, List[HCursor => ACursor]) => HCursor
  ): HCursor = {
    val topLevelCursors: List[HCursor => ACursor] = replyCursorOps(topLevelExprData.cursorOps)
    topLevelOperation(smartStringOperation(json.hcursor, smartStringCursors), topLevelCursors)
  }

  private def replyCursorOps(cursorOps: List[List[CursorOp]]): List[HCursor => ACursor] =
    cursorOps.map(history => (cursor: HCursor) => cursor.replay(history))

  private def modifyTopLevelExpression(
    topCursor: HCursor,
    cursors: List[HCursor => ACursor]
  )(
    manipulateExpr: (PathWithTranslatableConstants, String, String) => String
  ): HCursor =
    cursors
      .foldLeft(topCursor) { case (acc, f) =>
        val aCursor = f(acc)
        aCursor.withFocus { json =>
          val path = CursorOp.opsToPath(aCursor.history)
          val maybeTranslationState: Option[PathWithTranslatableConstants] = topLevelExprData.forPath(path)

          maybeTranslationState match {
            case None => json
            case Some(translationState) =>
              aCursor.as[String].toOption.fold(json) { currentExpr0 =>
                val finalExpr = manipulateExpr(translationState, currentExpr0, path)
                Json.fromString(finalExpr)
              }
          }
        }.root
      }

  private def translateExpressions(
    rows: List[Row]
  )(topCursor: HCursor, cursors: List[HCursor => ACursor]): HCursor =
    modifyTopLevelExpression(topCursor, cursors) { case (translationState, expr, path) =>
      val rowsForPath = rows.filter(_.path === path).distinct
      rowsForPath.foldRight(expr) { case (row, currentExpr) =>
        translationState.constants.find(_.enString === row.en) match {
          case None => currentExpr
          case Some(TranslatableConstant.NonTranslated(Constant(en))) =>
            currentExpr.replace(s"'$en'", s"'${row.en}'|'${row.cy}'")
          case Some(TranslatableConstant.Translated(Constant(en), Constant(cy))) =>
            currentExpr.replaceAll(s"'$en'\\s*\\|\\s*'$cy'", s"'${row.en}'|'${row.cy}'")
        }
      }
    }

  private def toTranslateJsonDebugTopLevelExpression(topCursor: HCursor, cursors: List[HCursor => ACursor]): HCursor =
    modifyTopLevelExpression(topCursor, cursors) { case (translationState, currentExpr, _) =>
      translationState.constants.foldRight(currentExpr) { case (translatableConstant, currentExprAcc) =>
        translatableConstant match {
          case TranslatableConstant.NonTranslated(Constant(en)) =>
            currentExprAcc.replace(s"'$en'", s"'✅'")
          case TranslatableConstant.Translated(Constant(en), Constant(cy)) =>
            currentExprAcc.replaceAll(s"'$en'\\s*\\|\\s*'$cy'", s"'✅'")
        }
      }
    }

  private def toTranslateJson(
    rows: List[Row]
  )(topCursor: HCursor, cursors: List[HCursor => ACursor]): HCursor = {
    val lookup: Map[String, Row] = rows.map(row => row.path -> row).toMap
    cursors
      .foldLeft(topCursor) { case (acc, f) =>
        val aCursor = f(acc)
        aCursor.withFocus { json =>
          val path = CursorOp.opsToPath(aCursor.history)
          if (json.isArray) {
            json
          } else {
            val attemptLang = aCursor.as[Lang]
            val attemptString = aCursor.as[String]
            lookup.get(path) match {
              case None => json // Ignore missing translation
              case Some(row) =>
                val tran = Json.obj(
                  "en" -> Json.fromString(row.en),
                  "cy" -> Json.fromString(row.cy)
                )
                if (attemptString.isRight) tran
                else if (attemptLang.isRight) json.deepMerge(tran)
                else throw new Exception(s"Cannot translate path: $path. Invalid focus: $json")
            }

          }
        }.root
      }
  }

  private def toTranslateJsonDebug(topCursor: HCursor, cursors: List[HCursor => ACursor]): HCursor =
    cursors
      .foldLeft(topCursor) { case (acc, f) =>
        val aCursor = f(acc)
        aCursor.withFocus { json =>
          Json.fromString("✅") // Useful for debugging to quickly spot what is translated
        }.root
      }

  private def smartStringsRows(cursors: List[HCursor => ACursor]): List[Row] =
    cursors
      .foldLeft(List.empty[Row]) { case (rows, f) =>
        val aCursor = f(json.hcursor)
        val attemptLang = aCursor.as[Lang]
        val attemptString = aCursor.as[String]
        val path = CursorOp.opsToPath(aCursor.history)
        attemptString
          .map { en =>
            Row(path, TextExtractor.escapeNewLine(en), "") :: rows
          }
          .orElse(attemptLang.map { lang =>
            Row(path, TextExtractor.escapeNewLine(lang.en), TextExtractor.escapeNewLine(lang.cy)) :: rows
          })
          .toOption
          .getOrElse(rows)
          .sortBy(_.path)

      }
      .distinct

  def fetchRows: List[Row] = smartStringsRows(smartStringCursors) ++ topLevelExprData.toRows

  def rowsForTranslation: List[TranslatedRow] = fetchRows
    .filterNot(row => ExtractAndTranslate(row.en).translateTexts.isEmpty)
    .map(row => TranslatedRow(row.en, row.cy))
    .distinct
    .sortBy(_.en)

  def untranslatedRowsForTranslation: List[EnTextToTranslate] = fetchRows
    .filterNot(row => row.cy.trim.nonEmpty) // Do not send to translation what has a welsh in json
    .flatMap(row => ExtractAndTranslate(row.en).translateTexts)
    .distinct
    .sortBy(_.en)

  def translateJson(
    expressionRows: List[Row]
  ): Json =
    applyOperations(
      toTranslateJson(expressionRows),
      translateExpressions(expressionRows)
    ).focus.get

  def translateJsonDebug: Json =
    applyOperations(
      toTranslateJsonDebug,
      toTranslateJsonDebugTopLevelExpression
    ).focus.get
}

object TextExtractor {

  def unescapeNewLine(s: String) = s.replaceAll("\\\\n", "\n")
  def escapeNewLine(s: String) = s.replace("\n", "\\n")

  import Instruction._

  def pathForSection(root: List[Instruction]): List[List[Instruction]] = {
    val confirmationQuestion = Pure(DownField("question")) :: Pure(DownField("confirmation")) :: root
    val addAnotherQuestion = Pure(DownField("addAnotherQuestion")) :: root
    val descriptionTotal = Pure(DownField("descriptionTotal")) :: root
    val description = Pure(DownField("description")) :: root
    List(
      Pure(DownField("title")) :: root,
      Pure(DownField("noPIITitle")) :: root,
      Pure(DownField("shortName")) :: root,
      Pure(DownField("errorShortName")) :: root,
      Pure(DownField("errorShortNameStart")) :: root,
      Pure(DownField("errorExample")) :: root,
      Pure(DownField("progressIndicator")) :: root,
      Pure(DownField("summaryName")) :: root,
      description,
      TraverseArray :: Pure(DownField("key")) :: description,
      TraverseArray :: Pure(DownField("value")) :: description,
      TraverseArray :: Pure(DownField("key")) :: descriptionTotal,
      TraverseArray :: Pure(DownField("value")) :: descriptionTotal,
      Pure(DownField("summaryDescription")) :: root,
      Pure(DownField("caption")) :: root,
      Pure(DownField("continueLabel")) :: root,
      Pure(DownField("label")) :: confirmationQuestion,
      TraverseArray :: Pure(DownField("choices")) :: confirmationQuestion,
      Pure(DownField("label")) :: addAnotherQuestion,
      Pure(DownField("errorMessage")) :: addAnotherQuestion
    )
  }

  def pathForInfoOnlyField(root: List[Instruction]): List[List[Instruction]] =
    List(
      Pure(DownField("label")) :: root,
      Pure(DownField("infoText")) :: root
    )

  def pathForField(root: List[Instruction]): List[List[Instruction]] = {
    val miniSummaryList = List(TraverseArray, Pure(DownField("rows"))) ++ root
    List(
      Pure(DownField("label")) :: root,
      Pure(DownField("infoText")) :: root,
      Pure(DownField("noneChoiceError")) :: root,
      Pure(DownField("dividerText")) :: root,
      Pure(DownField("shortName")) :: root,
      Pure(DownField("errorShortName")) :: root,
      Pure(DownField("errorShortNameStart")) :: root,
      Pure(DownField("errorExample")) :: root,
      Pure(DownField("helpText")) :: root,
      Pure(DownField("errorMessage")) :: root,
      Pure(DownField("repeatLabel")) :: root,
      Pure(DownField("repeatAddAnotherText")) :: root,
      Pure(DownField("chooseAddressLabel")) :: root,
      Pure(DownField("confirmAddressLabel")) :: root,
      Pure(DownField("summaryValue")) :: root,
      TraverseArray :: Pure(DownField("header")) :: root,
      Pure(DownField("value")) :: TraverseArray :: Pure(DownField("values")) :: TraverseArray :: Pure(
        DownField("rows")
      ) :: root,
      List(Pure(DownField("errorMessage")), TraverseArray, Pure(DownField("validators"))) ++ root,
      List(TraverseArray, Pure(DownField("choices"))) ++ root,
      List(TraverseArray, Pure(DownField("optionHelpText"))) ++ root,
      List(TraverseArray, Pure(DownField("hints"))) ++ root,
      Pure(DownField("key")) :: miniSummaryList
    )
  }

  def pathForCyaPage(root: List[Instruction]): List[List[Instruction]] =
    List(
      Pure(DownField("title")) :: root,
      Pure(DownField("caption")) :: root,
      Pure(DownField("updateTitle")) :: root,
      Pure(DownField("noPIITitle")) :: root,
      Pure(DownField("noPIIUpdateTitle")) :: root,
      Pure(DownField("header")) :: root,
      Pure(DownField("footer")) :: root,
      Pure(DownField("continueLabel")) :: root
    )

  def pathForSummarySection(root: List[Instruction]): List[List[Instruction]] =
    List(
      Pure(DownField("title")) :: root,
      Pure(DownField("header")) :: root,
      Pure(DownField("footer")) :: root,
      Pure(DownField("continueLabel")) :: root
    )

  def pathForEmailAuth(root: List[Instruction]): List[List[Instruction]] =
    List(
      Pure(DownField("emailUseInfo")) :: root,
      Pure(DownField("emailCodeHelp")) :: root,
      Pure(DownField("emailConfirmation")) :: root
    )

  val exitPages = List(TraverseArray, Pure(DownField("exitPages")))
  val sections = List(TraverseArray, Pure(DownField("sections")))
  val fields = List(TraverseArray, Pure(DownField("fields"))) ++ sections
  val groupFields = List(TraverseArray, Pure(DownField("fields"))) ++ fields
  val ackPdf = List(Pure(DownField("pdf")), Pure(DownField("acknowledgementSection")))
  val ackFields = List(TraverseArray, Pure(DownField("fields")), Pure(DownField("acknowledgementSection")))
  val decFields = List(TraverseArray, Pure(DownField("fields")), Pure(DownField("declarationSection")))
  val summarySection = List(Pure(DownField("summarySection")))
  val referrerConfig = List(Pure(DownField("referrerConfig")))
  val summarySectionFields = List(TraverseArray, Pure(DownField("fields"))) ++ summarySection
  val tasks = List(TraverseArray, Pure(DownField("tasks"))) ++ sections
  val taskSummarySection = List(Pure(DownField("summarySection"))) ++ tasks
  val taskSummarySectionFields = List(TraverseArray, Pure(DownField("fields"))) ++ taskSummarySection
  val atlLimitField = List(Pure(DownField("field")), Pure(DownField("limit"))) ++ sections
  val atlPages = List(TraverseArray, Pure(DownField("pages"))) ++ sections
  val atlDefaultPage = List(Pure(DownField("defaultPage"))) ++ sections
  val atlCyaPage = List(Pure(DownField("cyaPage"))) ++ sections
  val atlDefaultPageFields = List(TraverseArray, Pure(DownField("fields"))) ++ atlDefaultPage
  val atlPagesFields = List(TraverseArray, Pure(DownField("fields"))) ++ atlPages
  val tasksSections = List(TraverseArray, Pure(DownField("sections"))) ++ tasks
  val tasksFields = List(TraverseArray, Pure(DownField("fields"))) ++ tasksSections
  val tasksDefaultPage = Pure(DownField("defaultPage")) :: tasksSections
  val tasksCyaPage = Pure(DownField("cyaPage")) :: tasksSections
  val tasksDefaultPageFields = List(TraverseArray, Pure(DownField("fields"))) ++ tasksDefaultPage
  val tasksAtlLimitField = List(Pure(DownField("field")), Pure(DownField("limit"))) ++ tasksSections
  val tasksAtlPages = List(TraverseArray, Pure(DownField("pages"))) ++ tasksSections
  val tasksAtlPagesFields = List(TraverseArray, Pure(DownField("fields"))) ++ tasksAtlPages
  val authConfig = List(Pure(DownField("authConfig")))
  val configs = List(TraverseArray, Pure(DownField("configs"))) ++ authConfig
  val enrolmentSection = List(Pure(DownField("enrolmentSection"))) ++ authConfig
  val enrolmentSectionFields = List(TraverseArray, Pure(DownField("fields"))) ++ enrolmentSection
  val allFields =
    List(fields, atlPagesFields, tasksFields, tasksAtlPagesFields, enrolmentSectionFields, decFields, groupFields)
  val allRevealingFields = allFields.map { fields =>
    List(TraverseArray, TraverseArray, Pure(DownField("revealingFields"))) ++ fields
  }
  val gformPaths: List[List[Instruction]] =
    List(
      List(Pure(DownField("formName"))),
      List(Pure(DownField("description"))),
      Pure(DownField("label")) :: exitPages,
      Pure(DownField("exitMessage")) :: exitPages,
      Pure(DownField("title")) :: referrerConfig,
      Pure(DownField("exitMessage")) :: referrerConfig,
      List(Pure(DownField("shortName")), Pure(DownField("declarationSection"))),
      List(Pure(DownField("errorShortName")), Pure(DownField("declarationSection"))),
      List(Pure(DownField("errorShortNameStart")), Pure(DownField("declarationSection"))),
      List(Pure(DownField("errorExample")), Pure(DownField("declarationSection"))),
      List(Pure(DownField("title")), Pure(DownField("declarationSection"))),
      List(Pure(DownField("continueLabel")), Pure(DownField("declarationSection"))),
      List(Pure(DownField("shortName")), Pure(DownField("acknowledgementSection"))),
      List(Pure(DownField("errorShortName")), Pure(DownField("acknowledgementSection"))),
      List(Pure(DownField("errorShortNameStart")), Pure(DownField("acknowledgementSection"))),
      List(Pure(DownField("errorExample")), Pure(DownField("acknowledgementSection"))),
      List(Pure(DownField("title")), Pure(DownField("acknowledgementSection"))),
      List(Pure(DownField("label")), Pure(DownField("submitSection"))),
      List(Pure(DownField("taskLabel")), Pure(DownField("submitSection"))),
      Pure(DownField("header")) :: ackPdf,
      Pure(DownField("footer")) :: ackPdf,
      Pure(DownField("title")) :: tasks,
      Pure(DownField("title")) :: enrolmentSection
    ) ++
      pathForSection(sections) ++
      pathForSection(atlPages) ++
      pathForSection(tasksSections) ++
      pathForSection(atlDefaultPage) ++
      pathForSection(tasksDefaultPage) ++
      pathForSection(tasksAtlPages) ++
      allFields.flatMap(pathForField) ++
      allRevealingFields.flatMap(pathForField) ++
      pathForInfoOnlyField(ackFields) ++
      pathForInfoOnlyField(atlDefaultPageFields) ++
      pathForInfoOnlyField(atlLimitField) ++
      pathForInfoOnlyField(summarySectionFields) ++
      pathForInfoOnlyField(taskSummarySectionFields) ++
      pathForInfoOnlyField(tasksDefaultPageFields) ++
      pathForInfoOnlyField(tasksAtlLimitField) ++
      pathForCyaPage(atlCyaPage) ++
      pathForCyaPage(tasksCyaPage) ++
      pathForSummarySection(summarySection) ++
      pathForSummarySection(taskSummarySection) ++
      pathForEmailAuth(authConfig) ++
      pathForEmailAuth(configs)

  private def prepareRows(rows: List[Row]): List[List[String]] = {
    val rowsString: List[List[String]] = rows.sortBy(_.path).map { row =>
      List(row.path, row.en, row.cy)
    }
    List("path", "en", "cy") :: rowsString
  }

  private def prepareTranslatebleRows(rows: List[TranslatedRow]): List[(String, String)] = {
    val rowsString: List[(String, String)] = rows.sortBy(_.en).map { row =>
      (row.en, row.cy)
    }
    ("en", "cy") :: rowsString
  }

  private def writeCvsToOutputStream(rows: List[Row], bos: BufferedOutputStream): Unit = {
    val writer = CSVWriter.open(bos)
    writer.writeAll(prepareRows(rows))
    writer.close()
  }

  private def writeTranslatableCvsToOutputStream(rows: List[TranslatedRow], bos: BufferedOutputStream): Unit = {
    val writer = CSVWriter.open(bos)
    writer.writeAll(prepareTranslatebleRows(rows).map { case (en, cy) => List(en, cy) })
    writer.close()
  }

  private def writeEnTextToTranslateCvsToOutputStream(
    rows: List[EnTextToTranslate],
    bos: BufferedOutputStream
  ): Unit = {
    val writer = CSVWriter.open(bos)
    writer.writeAll(rows.sortBy(_.en).map(en => List(en.en)))
    writer.close()
  }

  private def readRows(reader: CSVReader): Spreadsheet = {
    val rows = reader.all().map {
      case en :: cy :: Nil =>
        (EnFromSpreadsheet(en.trim), CyFromSpreadsheet(cy.trim))
      case rawRow => throw new Exception(s"Input csv contains more columns than expected: $rawRow")
    }
    val spreadsheetRows = rows.drop(1) // Remove headers
    Spreadsheet(spreadsheetRows.toMap)
  }

  def readCvsFromString(csv: String): Spreadsheet = {
    val sr: StringReader = new StringReader(csv)
    val br: BufferedReader = new BufferedReader(sr)
    val reader = CSVReader.open(br)
    val spreadsheet = readRows(reader)
    reader.close()
    br.close()
    sr.close()
    spreadsheet
  }

  private def translateRows(
    spreadsheet: Spreadsheet,
    rows: List[Row],
    topLevelExprData: TopLevelExprData
  ): (List[MissingRow], List[Row]) = {
    val isTopLevelExpression: Set[String] = topLevelExprData.paths
    rows.foldLeft((List.empty[MissingRow], List.empty[Row])) { case ((missingRows, rows), row) =>
      val extractAndTranslate: ExtractAndTranslate = ExtractAndTranslate(row.en)

      if (extractAndTranslate.isTranslateable(spreadsheet)) {
        val welshTranslation: String = extractAndTranslate.translate(spreadsheet)
        (missingRows, row.copy(en = unescapeNewLine(row.en), cy = unescapeNewLine(welshTranslation)) :: rows)
      } else {
        if (row.cy.nonEmpty || isTopLevelExpression(row.path)) {
          (missingRows, rows)
        } else {
          (MissingRow(row.path, row.en) :: missingRows, row :: rows)
        }
      }
    }
  }

  def translateFile(spreadsheet: Spreadsheet, jsonString: String): (String, Json) = {
    val json: Json = parse(jsonString).toOption.get

    val translator = Translator(json, gformPaths)

    val rows: List[Row] = translator.fetchRows

    val (missingRows, translatedRows): (List[MissingRow], List[Row]) =
      translateRows(spreadsheet, rows, translator.topLevelExprData)

    val translatedCount = translatedRows.count(row => !row.en.isEmpty)
    val untranslatedCount = missingRows.size

    val stats = Json.obj(
      "translatedCount" := translatedCount,
      "untranslatedCount" := untranslatedCount,
      "untranslatedRows" := missingRows.map { mr =>
        Json.obj(
          "path" := mr.path,
          "en" := mr.en
        )
      }
    )

    val translatedJson = translator.translateJson(translatedRows)
    (translatedJson.spaces2.replaceAll(" :", ":"), stats)
  }

  def generateCvsFromString(source: String, bos: BufferedOutputStream): Unit =
    parse(source).toOption.fold(()) { json =>
      val rows = Translator(json, gformPaths).fetchRows
      writeCvsToOutputStream(rows, bos)
    }

  def generateTranslatableCvsFromString(source: String, bos: BufferedOutputStream): Unit =
    parse(source).toOption.fold(()) { json =>
      val rows = Translator(json, gformPaths).rowsForTranslation
      writeTranslatableCvsToOutputStream(rows, bos)
    }

  def generateBriefTranslatableCvsFromString(source: String, bos: BufferedOutputStream): Unit =
    parse(source).toOption.fold(()) { json =>
      val rows = Translator(json, gformPaths).untranslatedRowsForTranslation
      writeEnTextToTranslateCvsToOutputStream(rows, bos)
    }

  def generateTranslatableRows(source: String): List[(String, String)] =
    parse(source).toOption.fold(List.empty[(String, String)]) { json =>
      val rows = Translator(json, gformPaths).rowsForTranslation
      prepareTranslatebleRows(rows)
    }

  def generateBriefTranslatableRows(source: String): List[EnTextToTranslate] =
    parse(source).toOption.fold(List.empty[EnTextToTranslate]) { json =>
      Translator(json, gformPaths).untranslatedRowsForTranslation
    }

  def debug(source: String): String =
    parse(source).toOption.fold("") { json =>
      val translator = Translator(json, gformPaths)
      translator.translateJsonDebug.spaces2
    }

}
