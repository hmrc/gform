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
import julienrf.json.derived
import play.api.libs.json.{ JsError, OFormat, Reads }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils._
import uk.gov.hmrc.gform.sharedmodel.SmartString

sealed trait FormKind extends Product with Serializable {
  def fold[B](f: FormKind.Classic => B)(g: FormKind.TaskList => B): B =
    this match {
      case n: FormKind.Classic  => f(n)
      case r: FormKind.TaskList => g(r)
    }

  def foldNested[B](f: NonEmptyList[TaskSection] => B)(g: List[Section] => B): B =
    fold(classic => g(classic.sections))(taskList => f(taskList.sections))

  val allSections: List[Section] = foldNested(_.flatMap(_.sections).toList)(identity)

}

object FormKind {

  final case class TaskList(sections: NonEmptyList[TaskSection]) extends FormKind

  object TaskList {
    implicit val format: OFormat[TaskList] = derived.oformat()
  }

  final case class Classic(sections: List[Section]) extends FormKind

  object Classic {
    implicit val format: OFormat[Classic] = derived.oformat()
  }

  private val templateReads: Reads[FormKind] = Reads { json =>
    (json \ "type").validate[String].asOpt match {
      case Some("taskList") => (json \ "sections").validate[NonEmptyList[TaskSection]].map(FormKind.TaskList.apply)
      case Some("classic")  => (json \ "sections").validate[List[Section]].map(FormKind.Classic.apply)
      case unknown =>
        JsError(s"Internal gform error: unknown kind. Expected 'taskList' or 'classic', but got: $unknown")
    }
  }

  implicit val format: OFormat[FormKind] =
    OFormatWithTemplateReadFallback(templateReads)

  implicit val leafExprs: LeafExpr[FormKind] = (path: TemplatePath, t: FormKind) =>
    t match {
      case n: FormKind.TaskList => LeafExpr(path, n.sections)
      case r: FormKind.Classic  => LeafExpr(path, r.sections)
    }
}

final case class TaskSection(
  title: SmartString,
  tasks: NonEmptyList[Task]
) {
  val sections: NonEmptyList[Section] = tasks.flatMap(_.sections)
}
object TaskSection {
  implicit val format: OFormat[TaskSection] = derived.oformat()

  implicit val leafExprs: LeafExpr[TaskSection] = (path: TemplatePath, t: TaskSection) =>
    LeafExpr(path + "title", t.title) ++
      LeafExpr(path + "tasks", t.tasks)
}

final case class Task(
  title: SmartString,
  sections: NonEmptyList[Section],
  summarySection: Option[SummarySection],
  declarationSection: Option[DeclarationSection],
  includeIf: Option[IncludeIf],
  caption: Option[SmartString]
)

object Task {
  implicit val format: OFormat[Task] = derived.oformat()

  implicit val leafExprs: LeafExpr[Task] = (path: TemplatePath, t: Task) =>
    LeafExpr(path + "title", t.title) ++
      LeafExpr(path + "sections", t.sections) ++
      LeafExpr(path + "summarySection", t.summarySection) ++
      LeafExpr(path + "declarationSection", t.declarationSection) ++
      LeafExpr(path + "includeIf", t.includeIf) ++
      LeafExpr(path + "caption", t.caption)
}
