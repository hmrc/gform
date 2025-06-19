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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.json.{ Format, JsError, JsString, JsSuccess, Reads, Writes }

sealed trait SectionNumber extends Product with Serializable {

  def value: String = this match {
    case SectionNumber.Classic.NormalPage(TemplateSectionIndex(sectionIndex)) => "n" + sectionIndex.toString
    case SectionNumber.Classic.AddToListPage.DefaultPage(TemplateSectionIndex(sectionIndex)) =>
      "ad" + sectionIndex.toString
    case SectionNumber.Classic.AddToListPage.Page(TemplateSectionIndex(sectionIndex), iterationNumber, pageNumber) =>
      "ap" + sectionIndex.toString + "." + iterationNumber.toString + "." + pageNumber.toString
    case SectionNumber.Classic.AddToListPage.CyaPage(TemplateSectionIndex(sectionIndex), iterationNumber) =>
      "ac" + sectionIndex.toString + "." + iterationNumber.toString
    case SectionNumber.Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(sectionIndex), iterationNumber) =>
      "ar" + sectionIndex.toString + "." + iterationNumber.toString
    case SectionNumber.Classic.AddToListPage.DeclarationPage(TemplateSectionIndex(sectionIndex), iterationNumber) =>
      "as" + sectionIndex.toString + "." + iterationNumber.toString
    case SectionNumber.Classic.RepeatedPage(TemplateSectionIndex(sectionIndex), pageNumber) =>
      "r" + sectionIndex.toString + "." + pageNumber.toString
    case SectionNumber.TaskList(Coordinates(taskSectionNumber, taskNumber), sectionNumber) =>
      List(taskSectionNumber.value.toString, taskNumber.value.toString, sectionNumber.value).mkString(",")
  }

}

object SectionNumber {

  sealed trait Classic extends SectionNumber with Product with Serializable {
    def sectionIndex: TemplateSectionIndex
  }
  object Classic {
    case class NormalPage(sectionIndex: TemplateSectionIndex) extends Classic
    case class RepeatedPage(sectionIndex: TemplateSectionIndex, pageNumber: Int) extends Classic
    sealed trait AddToListPage extends Classic

    object AddToListPage {
      case class DefaultPage(sectionIndex: TemplateSectionIndex) extends AddToListPage
      case class Page(sectionIndex: TemplateSectionIndex, iterationNumber: Int, pageNumber: Int) extends AddToListPage
      case class CyaPage(sectionIndex: TemplateSectionIndex, iterationNumber: Int) extends AddToListPage
      case class RepeaterPage(sectionIndex: TemplateSectionIndex, iterationNumber: Int) extends AddToListPage
      case class DeclarationPage(sectionIndex: TemplateSectionIndex, iterationNumber: Int) extends AddToListPage
    }
  }

  final case class TaskList(
    coordinates: Coordinates,
    sectionNumber: Classic
  ) extends SectionNumber

  implicit val format: Format[SectionNumber.Classic] = Format[SectionNumber.Classic](
    Reads[SectionNumber.Classic] {
      case JsString(value) =>
        parseClassic(value) match {
          case Some(sectionNumber) => JsSuccess(sectionNumber)
          case None                => JsError(s"Invalid section number: $value")
        }
      case unknown => JsError(s"JsString value expected, got: $unknown")
    },
    Writes[SectionNumber.Classic](a => JsString(a.value))
  )

  // format: off
  private val NormalPageRegex                  = "^n(\\d+)$".r
  private val AddToListDefaultPageRegex        = "^ad(\\d+)$".r
  private val AddToListPageRegex               = "^ap(\\d+)\\.(\\d+)\\.(\\d+)$".r
  private val AddToListCyaPageRegex            = "^ac(\\d+)\\.(\\d+)$".r
  private val AddToListRepeaterPageRegex       = "^ar(\\d+)\\.(\\d+)$".r
  private val AddToListDeclarationSectionRegex = "^as(\\d+)\\.(\\d+)$".r
  private val RepeatedPageRegex                = "^r(\\d+)\\.(\\d+)$".r
  // format: on

  def parseClassic(string: String): Option[SectionNumber.Classic] =
    string match {
      case NormalPageRegex(sectionIndex) =>
        Some(SectionNumber.Classic.NormalPage(TemplateSectionIndex(sectionIndex.toInt)))
      case AddToListDefaultPageRegex(sectionIndex) =>
        Some(SectionNumber.Classic.AddToListPage.DefaultPage(TemplateSectionIndex(sectionIndex.toInt)))
      case AddToListPageRegex(sectionIndex, iterationNumber, pageNumber) =>
        Some(
          SectionNumber.Classic.AddToListPage
            .Page(TemplateSectionIndex(sectionIndex.toInt), iterationNumber.toInt, pageNumber.toInt)
        )
      case AddToListCyaPageRegex(sectionIndex, iterationNumber) =>
        Some(
          SectionNumber.Classic.AddToListPage
            .CyaPage(TemplateSectionIndex(sectionIndex.toInt), iterationNumber.toInt)
        )
      case AddToListRepeaterPageRegex(sectionIndex, iterationNumber) =>
        Some(
          SectionNumber.Classic.AddToListPage
            .RepeaterPage(TemplateSectionIndex(sectionIndex.toInt), iterationNumber.toInt)
        )
      case AddToListDeclarationSectionRegex(sectionIndex, iterationNumber) =>
        Some(
          SectionNumber.Classic.AddToListPage
            .DeclarationPage(TemplateSectionIndex(sectionIndex.toInt), iterationNumber.toInt)
        )
      case RepeatedPageRegex(sectionIndex, pageNumber) =>
        Some(SectionNumber.Classic.RepeatedPage(TemplateSectionIndex(sectionIndex.toInt), pageNumber.toInt))
      case _ => None
    }
}
