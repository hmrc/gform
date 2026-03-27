/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.handlebarstemplate

import cats.implicits.catsSyntaxEq
import play.api.libs.json.Json
import uk.gov.hmrc.gform.objectstore.Attachments
import uk.gov.hmrc.gform.sharedmodel.LangADT.En
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.HandlebarsTemplateProcessorModel
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ Field, FieldName, RoboticsXml, StructuredFormValue }
import uk.gov.hmrc.gform.submission.destinations.DestinationsProcessorModelAlgebra
import uk.gov.hmrc.gform.submission.handlebars.{ FocussedHandlebarsModelTree, HandlebarsModelTree }

import java.time.Instant
import java.time.temporal.ChronoUnit

object HandlebarsValidationHelpers {

  def extractTokensFromPayload(payload: String): Set[String] = {
    val pattern = """\{\{([^}]*)\}\}""".r
    pattern
      .findAllMatchIn(payload)
      .map(_.group(1))
      // Exclude Handlebars comments
      .filterNot(_.startsWith("!--"))
      // Remove string literals to avoid extracting tokens from them
      .map(_.replaceAll("'[^']+'", ""))
      // Exclude the special token "." which refers to the current context
      .filterNot(_ == ".")
      // Split on dot and dash to separate tokens (e.g., "user.name" -> "user", "name")
      .flatMap(_.split("[.-]"))
      // Replace special characters with space to separate tokens
      .map(_.replaceAll("[#/()';^=>*|]+", " "))
      // Split on whitespace to get individual tokens
      .flatMap(_.trim.split("\\s+"))
      // Exclude pure numeric tokens
      .filterNot(_.matches("^\\d+$"))
      // Exclude empty tokens
      .filterNot(_.isBlank)
      // Exclude Handlebars block parameters (e.g., "@index")
      .filterNot(_.startsWith("@"))
      .toSet
  }

  def extractSyntheticTokens(payload: String): Set[String] = {
    val tokenPattern = "\\{\\{\\s*#with[^}|]+\\|([a-zA-Z]+)\\|\\s*}}".r
    tokenPattern.findAllMatchIn(payload).map(_.group(1)).toList.filterNot(_ === ".").toSet
  }

  val knownHelpers =
    Set(
      // Handlebars built-in helpers
      "if",
      "unless",
      "each",
      "with",
      "lookup",
      "else",
      "log",
      // GForms custom helpers
      "either",
      "toDesDate",
      "yesNoToEtmpChoice",
      "dateToEtmpDate",
      "toEtmpDate",
      "toISO8601Date",
      "toISO8601DateTime",
      "either",
      "eitherExcludingBlanks",
      "isSuccessCode",
      "isNotSuccessCode",
      "desCurrentDate",
      "currentDate",
      "currentTimestamp",
      "currentMonth",
      "greaterThan",
      "lessThan",
      "equal",
      "isSigned",
      "isAccepted",
      "isAccepting",
      "isReturning",
      "isSubmitting",
      "toEtmpLegalStatus",
      "toEtmpDeclarationStatus",
      "match",
      "yesNoNull",
      "booleanToYesNo",
      "capitaliseFirst",
      "indexedLookup",
      "toDesAddressWithoutPostcodeFromArray",
      "toDesAddressWithoutPostcode",
      "removeEmptyAndGet",
      "elementAt",
      "stripCommas",
      "stripSpaces",
      "not",
      "or",
      "and",
      "isNull",
      "isNotNull",
      "toEtmpParamSequence",
      "toEtmpTelephoneNumber",
      "exists",
      "plus",
      "base64Encode",
      "normalisePostcode",
      "keyedLookup",
      "importBySubmissionReference",
      // Special additions/words
      "formId",
      "revealed",
      "street1",
      "street2",
      "street3",
      "street4",
      "line1",
      "line2",
      "line3",
      "line4",
      "city",
      "town",
      "postcode",
      "country",
      "as",
      "year",
      "month",
      "day",
      "choices",
      "choice",
      "this"
    )

  def mkSubmissionData(formTemplate: FormTemplate): SubmissionData =
    SubmissionData(
      PdfContent(""),
      None,
      FrontEndSubmissionVariables(Json.obj()),
      StructuredFormValue.ObjectStructure(mkStructuredFormData(formTemplate)),
      EmailParametersRecalculated(Map.empty),
      Attachments.empty,
      En,
      None,
      DestinationEvaluation.empty,
      UserSession.empty
    )

  def mkForm(formTemplate: FormTemplate): Form =
    Form(
      FormId("dummy-form-id"),
      EnvelopeId("dummy-envelope-id"),
      UserId("dummy-user-id"),
      formTemplate._id,
      None,
      FormData(fields = Seq.empty),
      InProgress,
      VisitIndex.empty(formTemplate.formKind),
      ThirdPartyData.empty,
      None,
      FormComponentIdToFileIdMapping.empty,
      TaskIdTaskStatusMapping.empty,
      Instant.now.truncatedTo(ChronoUnit.MILLIS)
    )

  def getAllFormFields(formTemplate: FormTemplate): List[String] =
    formTemplate.formKind.allSections.flatMap { section =>
      section.fold { page =>
        page.page.allFormComponentIds.map(_.value)
      } { repeatingPage =>
        repeatingPage.page.allFormComponentIds.map(_.value)
      } { addToList =>
        addToList.addAnotherQuestion.id.value :: addToList.pages.toList.flatMap(
          _.allFormComponentIds.map(_.value)
        )
      }
    } ++ formTemplate.expressionsOutput.fold(List.empty[String])(
      _.lookup.keys.map(_.id).toList
    ) ++ formTemplate.destinations.allFormComponents.map(_.id.value)

  def mkStructuredFormData(formTemplate: FormTemplate): List[Field] =
    formTemplate.formKind.allSections.flatMap { section =>
      section.fold { page =>
        page.expandedFormComponents().map(mkField) ++ page.page.confirmation.toList.map(_.question).map(mkField)
      } { repeatingPage =>
        repeatingPage.expandedFormComponents().map(mkArrayField) ++ repeatingPage.page.confirmation.toList
          .map(_.question)
          .map(mkArrayField)
      } { addToList =>
        List(
          Field(
            FieldName(addToList.addAnotherQuestion.id.value),
            StructuredFormValue.ArrayNode(
              StructuredFormValue.ObjectStructure(
                addToList.expandedFormComponents.map(mkField) ++ addToList.pages.toList.flatMap(
                  _.confirmation.toList.map(_.question).map(mkField)
                )
              ) :: Nil
            )
          )
        )
      }
    } ++ formTemplate.expressionsOutput.fold(List.empty[Field]) { exp =>
      exp.lookup.map { case (k, _) =>
        Field(FieldName(k.id), StructuredFormValue.TextNode("0"))
      }.toList
    } ++ formTemplate.destinations.allFormComponents.map(mkField)

  private def mkField(fc: FormComponent) =
    Field(
      FieldName(fc.id.value),
      mkValue(fc)
    )

  private def mkValue(fc: FormComponent) =
    fc match {
      case IsDate(_)            => mkDateValue
      case IsAddress(_)         => mkAddressValue
      case IsOverseasAddress(_) => mkAddressValue
      case IsPostcodeLookup(_)  => mkAddressValue
      case _                    => StructuredFormValue.TextNode("0")
    }

  private def mkDateValue: StructuredFormValue =
    StructuredFormValue.ObjectStructure(
      List(
        Field(FieldName("day"), StructuredFormValue.TextNode("01")),
        Field(FieldName("month"), StructuredFormValue.TextNode("12")),
        Field(FieldName("year"), StructuredFormValue.TextNode("2023"))
      )
    )

  private def mkAddressValue: StructuredFormValue =
    StructuredFormValue.ObjectStructure(
      List(
        Field(FieldName("street1"), StructuredFormValue.TextNode("line1"), Map(RoboticsXml -> FieldName("line1"))),
        Field(FieldName("street2"), StructuredFormValue.TextNode("line2"), Map(RoboticsXml -> FieldName("line2"))),
        Field(FieldName("street3"), StructuredFormValue.TextNode("line3"), Map(RoboticsXml -> FieldName("line3"))),
        Field(FieldName("street4"), StructuredFormValue.TextNode("line4"), Map(RoboticsXml -> FieldName("line4"))),
        Field(
          FieldName("postcode"),
          StructuredFormValue.TextNode("postcode"),
          Map(RoboticsXml -> FieldName("postcode"))
        ),
        Field(FieldName("country"), StructuredFormValue.TextNode("country"), Map(RoboticsXml -> FieldName("country")))
      )
    )

  private def mkArrayField(fc: FormComponent) =
    Field(
      FieldName(fc.id.value),
      StructuredFormValue.ArrayNode(
        List(
          mkValue(fc),
          mkValue(fc)
        )
      )
    )

  def mkFocusedTree(formTemplate: FormTemplate) = {
    val submissionData: SubmissionData = mkSubmissionData(formTemplate)
    val dummyForm: Form = mkForm(formTemplate)

    val templateProcessorModel: HandlebarsTemplateProcessorModel = DestinationsProcessorModelAlgebra
      .createModel(
        submissionData.variables,
        submissionData.pdfData,
        submissionData.instructionPDFData,
        submissionData.structuredFormData,
        dummyForm,
        None
      )

    val modelTree: HandlebarsModelTree = HandlebarsModelTree(
      dummyForm._id,
      SubmissionRef("dummy-submission-ref"),
      formTemplate,
      submissionData.pdfData,
      submissionData.instructionPDFData,
      submissionData.structuredFormData,
      templateProcessorModel
    )

    FocussedHandlebarsModelTree(modelTree, modelTree.value.model)
  }
}
