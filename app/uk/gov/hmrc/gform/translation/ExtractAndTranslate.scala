/*
 * Copyright 2025 HM Revenue & Customs
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

import cats.data.NonEmptyList
import cats.syntax.eq._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.nodes.Node
import org.jsoup.nodes.TextNode
import play.api.libs.json.JsString
import scala.jdk.StreamConverters._
import scala.util.matching.Regex
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartStringTemplateReader }

case class Replacement(from: String, to: String)

sealed trait TextType {
  def content: String
  def parent: Node
  def path: Option[String]
}

object TextType {
  final case class OwnText(textNode: TextNode, parent: Node, path: Option[String]) extends TextType {
    val content = textNode.getWholeText().replaceAll("\u00a0", "&nbsp;")
  }

  final case class InlineElement(element: Element, path: Option[String]) extends TextType {
    val replacements: List[Replacement] = element.nodeStream.toScala(List).drop(1).flatMap { node =>
      node.cast[TextNode].flatMap { textNode =>
        val from = textNode.outerHtml() // This contains escaped ampersand like in R&amp;D
        val to = textNode.getWholeText() // This contains original value for example: R&D
        if (from === to) {
          None
        } else {
          Some(Replacement(from, to))
        }
      }
    }

    val content = replacements.foldRight(element.outerHtml())((repl, acc) => acc.replace(repl.from, repl.to))
    val parent = element.parent()
  }

  final case class CompoundNode(textTypes: NonEmptyList[TextType]) extends TextType {
    val content = textTypes
      .map {
        case ot @ OwnText(_, _, _)    => ot.content
        case ie @ InlineElement(_, _) => ie.content
        case cn @ CompoundNode(_)     => cn.content
      }
      .toList
      .mkString("")
      .trim()
    val parent = textTypes.head.parent
    val path = textTypes.head.path
  }
}

final case class EnTextToTranslate(en: String, path: Seq[String])

final case class EnFromSpreadsheet(value: String)
final case class CyFromSpreadsheet(value: String)

object EnFromSpreadsheet {
  def apply(s: String): EnFromSpreadsheet = new EnFromSpreadsheet(s.replace("\\n", "\n"))
}
object CyFromSpreadsheet {
  def apply(s: String): CyFromSpreadsheet = new CyFromSpreadsheet(s.replace("\\n", "\n"))
}

class Spreadsheet(val rows: Map[EnFromSpreadsheet, CyFromSpreadsheet]) {
  private val normalisedMap: Map[String, CyFromSpreadsheet] = rows.map { case (EnFromSpreadsheet(en), cy) =>
    en.replaceAll("\"", "'") -> cy
  }

  def contains(text: String): Boolean = {
    val normalisedEnglish = text.replaceAll("\"", "'")
    normalisedMap.get(normalisedEnglish).fold(false)(_.value.nonEmpty)
  }

  def get(textType: TextType, lookup: Map[Int, String]): Option[CyFromSpreadsheet] = {
    val normalisedEnglish = textType.content.replaceAll("\"", "'")

    ExtractAndTranslate.markdownBreakdown(normalisedEnglish, lookup) match {
      case Nil         => None
      case head :: Nil => normalisedMap.get(head)
      case fragments =>
        val maybeCyMarkdown: Option[String] =
          fragments
            .sortBy(_.size)
            .reverse // Replace longer sentences first to avoid replacing bits of longer sentences by smaller one
            .foldLeft[Option[String]](Some(normalisedEnglish)) { case (acc, enFragment) =>
              val cyFragment: Option[CyFromSpreadsheet] = normalisedMap.get(enFragment)
              cyFragment.flatMap { cy =>
                acc.map { s =>
                  val translateeRemasked = ExprMasker.remask(s, lookup)
                  val fragmentRemasked = ExprMasker.remask(enFragment, lookup)
                  translateeRemasked.replace(fragmentRemasked, cy.value)
                }
              }
            }
        maybeCyMarkdown.map { welsh =>
          CyFromSpreadsheet(welsh)
        }
    }
  }
}

object Spreadsheet {
  def apply(rows: Map[EnFromSpreadsheet, CyFromSpreadsheet]): Spreadsheet = new Spreadsheet(rows)
}

trait ExtractAndTranslate {
  def root: Node
  def translateTexts: List[EnTextToTranslate]
  def translate(spreadsheet: Spreadsheet): String
  def isTranslateable(spreadsheet: Spreadsheet): Boolean
}

object ExtractAndTranslate {
  private val orderedListStarter: Regex = "^[0-9]+\\. .*".r
  private val startOfSentence: Regex = "^[A-Z].*".r

  // Assumption is that '\n' exists only by itself in the text, never
  // followed by another '\n'.
  def splitOrderedList(text: String): List[String] = {

    val cleanedText = if (text.startsWith("\n")) text.drop(1) else text
    val texts = cleanedText.split("\n").toList

    val (items, last) = texts.foldLeft((List.empty[String], "")) { case ((acc, last), token) =>
      if (orderedListStarter.matches(token)) { // New sentence begin
        (acc ++ List(last), token)
      } else {
        if (last.isEmpty) {
          (acc, token)
        } else {
          (acc, last + "\n" + token)
        }
      }
    }

    val beg = if (text.startsWith("\n") && !orderedListStarter.matches(cleanedText)) "\n" else ""
    val end = if (text.endsWith("\n")) "\n" else ""
    val res = items ++ List(last + end)

    res.zipWithIndex
      .map { case (s, index) =>
        if (index === 0) beg + s else s
      }
      .filter(_.trim.nonEmpty)
  }

  def splitWholeSentences(text: String): List[String] = {
    val texts: Array[String] = text.split("\\. ")

    val sentences = texts.zipWithIndex.map { case (s, index) =>
      if (index === texts.size - 1) {
        s
      } else {
        s + "."
      }
    }

    val (res, last) = sentences.foldLeft((List.empty[String], "")) { case ((acc, last), token) =>
      if (startOfSentence.matches(token) && last.size > 3) { // New sentence begin
        (acc ++ List(last), token)
      } else {
        if (last.isEmpty) {
          (acc, token)
        } else {
          (acc, last + " " + token)
        }
      }
    }

    res ++ List(last)
  }

  private val hasTextRegex = "(?s).*[a-zA-Z]+(?s).*".r

  def exprHasTranslatebleContent(text: String): Boolean =
    SmartStringTemplateReader.templateReads
      .reads(JsString(text))
      .fold(
        er => false,
        { s =>
          val constants = s.interpolations.flatMap(_.constants)
          val translatableConstants = constants.filter {
            case TranslatableConstant.NonTranslated(en) => hasTextRegex.matches(en.value)
            case TranslatableConstant.Translated(en, _) => false
          }
          translatableConstants.isEmpty // Is there any non-translated text constant
        }
      )

  def nothingToTranslate(s: String): Boolean = {

    val (redacted, expressions) = ExprMasker.mask(s)

    val redactedClean = redacted
      .replace("$n", "")
      .replaceAll(ExprMasker.primaryRegex, "")

    val noOwnText = !hasTextRegex.matches(redactedClean)

    val noTextInExpressions = expressions.values.forall(exprHasTranslatebleContent(_))

    noOwnText && noTextInExpressions
  }

  def singleInlineElement(text: String): String =
    if (text.startsWith("<")) {
      val (maskedText, secondaryLookup) = ExprMasker.mask(text)
      val document: Document = Jsoup.parseBodyFragment(maskedText)
      val body: Element = document.body()
      val singleElementText = isSingleInlineElement(body, 1).fold(text)(_.getWholeText().replaceAll("\u00a0", "&nbsp;"))
      ExprMasker.unmask(singleElementText, secondaryLookup)
    } else text

  def markdownBreakdown(text: String, lookup: Map[Int, String]): List[String] =
    text
      .split(
        // markdown lists with extra padding ('\n\n* ' or '\n\n- ' note the space at the end)
        "\\n\\s*\\n\\s*[*-]\\s+"
      )
      .flatMap(_.split("\\n\\s*\\n")) // markdown paragraphs (usually \n\n)
      .flatMap(_.split("\\n\\s*[*-]")) // markdown lists (usually \n* or \n-)
      .flatMap(_.split("^\\s*[\\*-]\\s+")) // markdown lists ('* ' or '- ' or '   * ' etc. at beggining of the sentence)
      .flatMap(_.split("\\n\\s*#+")) // markdown heading following new line (usually \n##)
      .flatMap(_.split("<br>"))
      .flatMap(s => splitOrderedList(s))
      .map(_.trim.dropWhile(_ === '#')) // Ignore all # at the beginning of the text
      .flatMap(s => splitWholeSentences(s))
      .filter(!_.isEmpty)
      .toList
      .map(s => ExprMasker.unmask(s, lookup))
      .map(s => singleInlineElement(s))
      .filter(s => !nothingToTranslate(s))
      .map(_.trim)

  private val numberRegex = """^([0-9]+)$""".r

  def isStandaloneExpr(text: String): Boolean =
    SmartStringTemplateReader.templateReads
      .reads(JsString(text))
      .fold(
        _ => false,
        { s =>
          val translatableConstants = s.interpolations.flatMap(_.constants).filter {
            case TranslatableConstant.NonTranslated(en) => !numberRegex.matches(en.value)
            case TranslatableConstant.Translated(en, _) => false
          }
          s.rawValue(LangADT.En).trim === "{0}" &&
          translatableConstants.isEmpty // Is there any non-translated text constant
        }
      )

  def isSingleInlineElement(element: Element, numberToDrop: Int): Option[TextNode] = {
    val nodes: List[Node] = element.nodeStream.toScala(List).drop(numberToDrop)

    nodes match {
      case inline :: text :: Nil =>
        (inline.cast[Element], text.cast[TextNode]) match {
          case (Some(el), Some(text)) if !el.isBlock() && el.nodeName() =!= "a" =>
            Some(text)
          case _ => None
        }
      case _ => None
    }
  }

  final class BlockHtmlElementsAndOwnTexts(
    nodes: List[TextType],
    val root: Node,
    val originalText: String,
    val lookup: Map[Int, String]
  ) extends ExtractAndTranslate {

    def nonTranslatable(text: String): Boolean = {
      val trimmedText = text.trim.replaceAll("\n", "")

      trimmedText.isEmpty || // do not translate empty strings
      (trimmedText match {
        case numberRegex(_) => true // do not translate numbers
        case _              => false
      }) ||
      isStandaloneExpr(text)
    }

    val translateTexts: List[EnTextToTranslate] =
      nodes.flatMap { node =>
        val breakdown = markdownBreakdown(node.content, lookup)
          .filterNot(nonTranslatable)
        breakdown.map(en => EnTextToTranslate(en, node.path.toSeq))
      }

    def isTranslateable(spreadsheet: Spreadsheet): Boolean =
      translateTexts.forall(enTextToTranslate => spreadsheet.contains(enTextToTranslate.en))

    def translate(spreadsheet: Spreadsheet): String = {
      nodes.foreach { node =>
        spreadsheet.get(node, lookup).foreach { cyFromSpreadsheet =>
          node match {
            case ot @ TextType.OwnText(textNode, parentNode, path) =>
              if (parentNode.nameIs("body")) {
                textNode.text(cyFromSpreadsheet.value)
              } else {
                parentNode.cast[Element].foreach { element =>
                  element.nodeStream.toScala(List).drop(1).foreach { child =>
                    if (child == textNode) {
                      child.before(cyFromSpreadsheet.value)
                      child.remove()
                    }
                  }
                }
              }
            case TextType.InlineElement(element, path) =>
              if (isSingleInlineElement(element, 0).isDefined) {
                element.html(cyFromSpreadsheet.value)
              } else {
                element.before(cyFromSpreadsheet.value)
                element.remove()
              }
            case TextType.CompoundNode(textTypes) =>
              val maybeHead = textTypes.toList.dropWhile {
                case TextType.OwnText(textNode, _, path)   => textNode.isBlank
                case TextType.InlineElement(element, path) => false
                case TextType.CompoundNode(_)              => false
              }.headOption

              maybeHead.foreach {
                case TextType.OwnText(textNode, _, path)   => textNode.before(cyFromSpreadsheet.value)
                case TextType.InlineElement(element, path) => element.before(cyFromSpreadsheet.value)
                case TextType.CompoundNode(_)              => // Do nothing
              }
              textTypes.head.parent.cast[Element].foreach { element =>
                element.nodeStream.toScala(List).drop(1).foreach { child =>
                  textTypes.toList.foreach {
                    case TextType.OwnText(textNode, _, path) if child == textNode && !textNode.isBlank() =>
                      child.remove()
                    case TextType.InlineElement(element, path) if child == element => child.remove()
                    case TextType.OwnText(_, _, path)                              => // Do nothing
                    case TextType.InlineElement(_, path)                           => // Do nothing
                    case TextType.CompoundNode(_)                                  => // Do nothing
                  }
                }
              }
          }
        }
      }

      // If even single node failed to translate result of translation must be empty string
      val replacements = root.nodeStream.toScala(List).drop(1).flatMap { node =>
        node.cast[TextNode].flatMap { textNode =>
          val from = textNode.outerHtml() // This contains escaped ampersand like in R&amp;D
          val to = textNode.getWholeText() // This contains original value for example: R&D
          if (from === to) {
            None
          } else {
            Some(Replacement(from, to))
          }
        }
      }
      val sb = new StringBuffer()
      root.html(sb)
      val resultHtml = sb.toString()
      val res = replacements.foldRight(resultHtml) { case (repl, acc) => acc.replace(repl.from, repl.to) }
      ExprMasker
        .unmask(res, lookup)
        .reverse // Due to the way html is manipulated it can happen there stays whitespaces at the end.
        .dropWhile(_ === ' ')
        .reverse
    }
  }

  def apply(rawEnglish: String, path: Option[String]): ExtractAndTranslate = {
    val (english, lookup) = ExprMasker.mask(rawEnglish)

    val document: Document = Jsoup.parseBodyFragment(english)
    document.outputSettings().prettyPrint(false) // Don't pretty print result of calls to .html() etc
    val body: Element = document.body()
    val nodes: List[Node] = body.nodeStream.toScala(List).drop(1)
    val treeNodes: List[TextType] = nodes.foldRight(List.empty[TextType]) { case (node, treeNodes) =>
      val parent: Node = node.parent()
      val parentAsElement: Option[Element] = node.parent().cast[Element]
      val parentIsBlock: Boolean = parentAsElement.exists(_.isBlock)
      val maybeText: Option[TextNode] = node.cast[TextNode]
      val nodeIsText: Boolean = maybeText.isDefined
      val maybeElement: Option[Element] = node.cast[Element]
      val elementIsBlock: Boolean = maybeElement.exists(_.isBlock)
      val elementIsInline: Boolean = maybeElement.exists(!_.isBlock)

      if (elementIsBlock) {
        treeNodes
      } else if (parentIsBlock && elementIsInline) {
        TextType.InlineElement(maybeElement.get, path) :: treeNodes
      } else if (parentIsBlock && nodeIsText) {
        TextType.OwnText(maybeText.get, parent, path) :: treeNodes
      } else {
        treeNodes
      }
    }

    val compoudedTreeNodes = compoud(treeNodes)
    new BlockHtmlElementsAndOwnTexts(compoudedTreeNodes, body, english, lookup)
  }

  def compoud(treeNodes: List[TextType]): List[TextType] =
    // If subsequent nodes has same parent we want to treat them like single english text to translate
    treeNodes match {
      case Nil         => Nil
      case head :: Nil => head :: Nil
      case (first @ TextType.CompoundNode(nel)) :: second :: tail =>
        if (first.parent == second.parent) compoud(TextType.CompoundNode(nel.append(second)) :: tail)
        else first :: compoud(second :: tail)

      case first :: second :: tail =>
        if (first.parent == second.parent) compoud(TextType.CompoundNode(NonEmptyList.of(first, second)) :: tail)
        else first :: compoud(second :: tail)
    }
}
