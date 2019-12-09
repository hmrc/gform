/*
 * Copyright 2019 HM Revenue & Customs
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

import play.api.libs.json._
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.formtemplate.FormComponentMaker
import uk.gov.hmrc.gform.sharedmodel.{ LabelHelper, SmartString }

case class ExpandedFormComponent(expandedFormComponent: List[FormComponent]) extends AnyVal

case class FormComponent(
  id: FormComponentId,
  `type`: ComponentType,
  label: SmartString,
  helpText: Option[SmartString],
  shortName: Option[SmartString],
  validIf: Option[ValidIf],
  mandatory: Boolean,
  editable: Boolean,
  submissible: Boolean,
  derived: Boolean,
  onlyShowOnSummary: Boolean = false,
  errorMessage: Option[SmartString],
  presentationHint: Option[List[PresentationHint]] = None
) {

  private def updateField(i: Int, fc: FormComponent): FormComponent =
    fc.copy(
      label = LabelHelper.buildRepeatingLabel(fc.label, i),
      shortName = LabelHelper.buildRepeatingLabel(fc.shortName, i))

  private def loop(fc: FormComponent): List[FormComponent] =
    fc.`type` match {
      case Group(fields, _, max, _, _, _) =>
        val expandedFields =
          for {
            field <- fields
            res <- updateField(1, field) :: (1 until max.getOrElse(1))
                    .map(i => updateField(i + 1, field.copy(id = FormComponentId(i + "_" + field.id.value))))
                    .toList
          } yield res
        expandedFields.flatMap(loop) // for case when there is group inside group (Note: it does not work, we would need to handle prefix)
      case _ => fc :: Nil
    }

  val expandFormComponent: ExpandedFormComponent = ExpandedFormComponent(loop(this))

}

object FormComponent {

  private val templateReads: Reads[FormComponent] = Reads(
    json => new FormComponentMaker(json).optFieldValue() fold (us => JsError(us.toString), fv => JsSuccess(fv)))

  implicit val format: OFormat[FormComponent] = OFormatWithTemplateReadFallback(templateReads)
}

object IsGroup {
  def unapply(fc: FormComponent): Option[Group] = fc.`type`.cast[Group]
}

object IsText {
  def unapply(fc: FormComponent): Option[Text] = fc.`type`.cast[Text]
}

object IsTextArea {
  def unapply(fc: FormComponent): Option[TextArea] = fc.`type`.cast[TextArea]
}

object IsRevealingChoice {
  def unapply(fc: FormComponent): Option[RevealingChoice] = fc.`type`.cast[RevealingChoice]
}
