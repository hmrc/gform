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
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartStringTemplateReader }

case class Replacement(from: String, to: String)

sealed trait TextType {
  def content: String
  def parent: Node
}

object TextType {
  final case class OwnText(textNode: TextNode, parent: Node) extends TextType {
    val wholeText = textNode.getWholeText().replaceAll("\u00a0", "&nbsp;")
    val content = wholeText.trim()
    val whiteSpaceStart = wholeText.takeWhile(c => c.isWhitespace)
    val whiteSpaceEnd = wholeText.reverse.takeWhile(c => c.isWhitespace).reverse
    val whiteSpaceEndSize = whiteSpaceEnd.size
  }

  final case class InlineElement(element: Element) extends TextType {
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
        case ot @ OwnText(_, _)    => ot.wholeText
        case ie @ InlineElement(_) => ie.content
        case cn @ CompoundNode(_)  => cn.content
      }
      .toList
      .mkString("")
      .trim()
    val parent = textTypes.head.parent
  }
}

trait ExtractAndTranslate {
  def root: Node
  def translateTexts: List[EnTextToTranslate]
  def translate(spreadsheet: Spreadsheet): String
  def isTranslateable(spreadsheet: Spreadsheet): Boolean
}

final case class EnFromSpreadsheet(value: String)
final case class CyFromSpreadsheet(value: String)

class Spreadsheet(val rows: Map[EnFromSpreadsheet, CyFromSpreadsheet]) {
  private val normalisedMap: Map[String, CyFromSpreadsheet] = rows.map { case (EnFromSpreadsheet(en), cy) =>
    en.replaceAll("\"", "'") -> cy
  }

  def contains(text: String): Boolean = {
    val normalisedEnglish = text.replaceAll("\"", "'")
    normalisedMap.contains(normalisedEnglish)
  }

  def get(textType: TextType): Option[CyFromSpreadsheet] = {
    val normalisedEnglish = textType.content.replaceAll("\"", "'")

    ExtractAndTranslate.markdownBreakdown(normalisedEnglish) match {
      case Nil         => None
      case head :: Nil => normalisedMap.get(head)
      case fragments =>
        val maybeCyMarkdown: Option[String] = fragments.foldLeft[Option[String]](Some(normalisedEnglish)) {
          case (acc, enFragment) =>
            val cyFragment: Option[CyFromSpreadsheet] = normalisedMap.get(enFragment)
            cyFragment.flatMap { cy =>
              acc.map(_.replace(enFragment, cy.value))
            }
        }
        maybeCyMarkdown.map(CyFromSpreadsheet(_))
    }
  }
}

object Spreadsheet {
  def apply(rows: Map[EnFromSpreadsheet, CyFromSpreadsheet]): Spreadsheet = new Spreadsheet(rows)
}

object ExtractAndTranslate {

  def markdownBreakdown(text: String): List[String] = text
    .split(
      // markdown lists with extra padding ('\n\n* ' or '\n\n- ' note the space at the end)
      """\\n\s*\\n\s*[\*-]\s+"""
    )
    .flatMap(_.split("""\\n\s*\\n""")) // markdown paragraphs (usually \n\n)
    .flatMap(_.split("""\\n\s*[\*-]""")) // markdown lists (usually \n* or \n-)
    .flatMap(_.split("""\\n\s*#+""")) // markdown heading following new line (usually \n##)
    .map { s =>
      val s1 = if (s.startsWith("""\n""")) {
        s.drop(2)
      } else s
      if (s1.endsWith("""\n""")) {
        s1.dropRight(2)
      } else s1
    }
    .map(_.trim.dropWhile(_ === '#')) // Ignore all # at the beginning of the text
    .map(_.trim)
    .filter(!_.isEmpty)
    .toList

  final class BlockHtmlElementsAndOwnTexts(nodes: List[TextType], val root: Node, val originalText: String)
      extends ExtractAndTranslate {

    def isStandaloneExpr(text: String): Boolean =
      SmartStringTemplateReader.templateReads
        .reads(JsString(text))
        .fold(
          _ => false,
          _.rawValue(LangADT.En).trim === "{0}"
        )

    def nonTranslatable(text: String): Boolean = {
      val numberRegex = """^([0-9]+)$""".r
      val trimmedText = text.trim.replaceAll("""\\n""", "")

      trimmedText.isEmpty || // do not translate empty strings
      (trimmedText match {
        case numberRegex(_) => true // do not translate numbers
        case _              => false
      }) ||
      isStandaloneExpr(text)
    }

    val translateTexts: List[EnTextToTranslate] =
      nodes
        .map(_.content)
        .filterNot(nonTranslatable)
        .flatMap(en => markdownBreakdown(en))
        .map(en => EnTextToTranslate(en))

    def isTranslateable(spreadsheet: Spreadsheet): Boolean =
      translateTexts.forall(enTextToTranslate => spreadsheet.contains(enTextToTranslate.en))

    def translate(spreadsheet: Spreadsheet): String = {
      nodes.zipWithIndex.foreach { case (node, index) =>
        spreadsheet.get(node).foreach { cyFromSpreadsheet =>
          node match {
            case ot @ TextType.OwnText(textNode, parentNode) =>
              if (parentNode.nameIs("body")) {
                textNode.text(ot.whiteSpaceStart + cyFromSpreadsheet.value + ot.whiteSpaceEnd)
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
            case TextType.InlineElement(element) =>
              element.before(cyFromSpreadsheet.value)
              element.remove()
            case TextType.CompoundNode(textTypes) =>
              val whiteSpaceEnd = textTypes.last match {
                case ot @ TextType.OwnText(textNode, _) => ot.whiteSpaceEnd
                case TextType.InlineElement(element)    => ""
                case TextType.CompoundNode(_)           => ""
              }
              textTypes.head match {
                case ot @ TextType.OwnText(textNode, _) =>
                  textNode.before(ot.whiteSpaceStart + cyFromSpreadsheet.value + whiteSpaceEnd)
                case TextType.InlineElement(element) => element.before(cyFromSpreadsheet.value)
                case TextType.CompoundNode(_)        => // Do nothing
              }
              textTypes.head.parent.cast[Element].foreach { element =>
                element.nodeStream.toScala(List).drop(1).foreach { child =>
                  textTypes.toList.foreach {
                    case TextType.OwnText(textNode, _) if child == textNode  => child.remove()
                    case TextType.InlineElement(element) if child == element => child.remove()
                    case TextType.OwnText(_, _)                              => // Do nothing
                    case TextType.InlineElement(_)                           => // Do nothing
                    case TextType.CompoundNode(_)                            => // Do nothing
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
      val resultHtml = sb.toString().trim()
      replacements.foldRight(resultHtml) { case (repl, acc) => acc.replace(repl.from, repl.to) }

    }
  }

  def apply(english: String): ExtractAndTranslate = {
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
        TextType.InlineElement(maybeElement.get) :: treeNodes
      } else if (parentIsBlock && nodeIsText) {
        TextType.OwnText(maybeText.get, parent) :: treeNodes
      } else {
        treeNodes
      }
    }

    val compoudedTreeNodes = compoud(treeNodes)

    new BlockHtmlElementsAndOwnTexts(compoudedTreeNodes, body, english)
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
