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

import cats.data.NonEmptyList
import play.api.libs.json.{ Format, Json }
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils._

case class CheckYourAnswersPage(
  title: Option[SmartString],
  caption: Option[SmartString],
  updateTitle: SmartString,
  noPIITitle: Option[SmartString],
  noPIIUpdateTitle: Option[SmartString],
  header: Option[SmartString],
  footer: Option[SmartString],
  continueLabel: Option[SmartString],
  presentationHint: Option[PresentationHint],
  removeItemIf: Option[RemoveItemIf],
  fields: Option[NonEmptyList[FormComponent]]
)

object CheckYourAnswersPage {
  implicit val format: Format[CheckYourAnswersPage] = Json.format[CheckYourAnswersPage]

  implicit val leafExprs: LeafExpr[CheckYourAnswersPage] = (path: TemplatePath, t: CheckYourAnswersPage) =>
    LeafExpr(path + "title", t.title) ++
      LeafExpr(path + "caption", t.caption) ++
      LeafExpr(path + "noPIITitle", t.noPIITitle) ++
      LeafExpr(path + "updateTitle", t.updateTitle) ++
      LeafExpr(path + "noPIIUpdateTitle", t.noPIIUpdateTitle) ++
      LeafExpr(path + "header", t.header) ++
      LeafExpr(path + "footer", t.footer) ++
      LeafExpr(path + "continueLabel", t.continueLabel) ++
      LeafExpr(path + "removeItemIf", t.removeItemIf) ++
      LeafExpr(path + "fields", t.fields)
}
