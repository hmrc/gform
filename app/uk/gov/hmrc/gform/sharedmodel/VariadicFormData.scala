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

package uk.gov.hmrc.gform.sharedmodel

import cats.instances.list._
import cats.syntax.foldable._
import cats.{ Monoid, Show }
import cats.syntax.show._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.collection.GenTraversableOnce

sealed trait VariadicValue extends Product with Serializable {
  def toSeq: Seq[String] = this match {
    case VariadicValue.One(v)   => Seq(v)
    case VariadicValue.Many(vs) => vs
  }

  def toSet: Set[String] = toSeq.toSet

  def exists(pred: String => Boolean): Boolean = toSeq.exists(pred)

  def contains(s: String): Boolean = toSeq.contains(s)

  def map(f: String => String): VariadicValue = this match {
    case VariadicValue.One(v)   => VariadicValue.One(f(v))
    case VariadicValue.Many(vs) => VariadicValue.Many(vs.map(f))
  }
}

object VariadicValue {
  case class One(value: String) extends VariadicValue
  case class Many(value: Seq[String]) extends VariadicValue

  implicit val show: Show[VariadicValue] = new Show[VariadicValue] {
    private def quote(s: String) = s""""$s""""
    override def show(t: VariadicValue): String = t match {
      case One(v)   => quote(v)
      case Many(vs) => "[" + vs.map(quote).mkString(", ") + "]"
    }
  }

  def isVariadic(componentType: ComponentType): Boolean = componentType match {
    case _: Choice          => true
    case _: RevealingChoice => true
    case _                  => false
  }
}

case class VariadicFormData(data: Map[FormComponentId, VariadicValue]) {
  def get(id: FormComponentId): Option[VariadicValue] = data.get(id)

  def keySet(): Set[FormComponentId] = data.keySet

  def ++(addend: VariadicFormData): VariadicFormData = VariadicFormData(data ++ addend.data)
  def addValue(entry: (FormComponentId, VariadicValue)): VariadicFormData = VariadicFormData(data + entry)
  def addOne(entry: (FormComponentId, String)): VariadicFormData =
    this addValue (entry._1 -> VariadicValue.One(entry._2))
  def addMany(entry: (FormComponentId, Seq[String])): VariadicFormData =
    this addValue (entry._1 -> VariadicValue.Many(entry._2))

  def --(formComponents: GenTraversableOnce[FormComponentId]): VariadicFormData =
    VariadicFormData(data -- formComponents)

  def collect[B](pf: PartialFunction[(FormComponentId, VariadicValue), B]): Iterable[B] = data.collect(pf)

  def contains(id: FormComponentId): Boolean = data.contains(id)

  def mapValues(f: (FormComponentId, VariadicValue) => VariadicValue): VariadicFormData =
    VariadicFormData(data.map {
      case (k, v) => (k, f(k, v))
    })

  def one(id: FormComponentId): Option[String] =
    get(id)
      .map {
        case VariadicValue.One(v) => v
        case notOne =>
          throw new IllegalArgumentException(
            show"""Expected VariadicValue.One for form component ID "$id". Got $notOne""")
      }

  def oneOrElse(id: FormComponentId, dflt: => String): String = one(id).getOrElse(dflt)

  def many(id: FormComponentId): Option[Seq[String]] =
    get(id)
      .map {
        case VariadicValue.Many(vs) => vs
        case notMany =>
          throw new IllegalArgumentException(
            show"""Expected VariadicValue.Many for form component ID "$id". Got $notMany""")
      }
}

object VariadicFormData {
  val empty: VariadicFormData = VariadicFormData(Map.empty)

  def create(idAndValue: (FormComponentId, VariadicValue)*): VariadicFormData =
    VariadicFormData(idAndValue.toMap)

  def one(formComponentId: FormComponentId, value: String): VariadicFormData =
    VariadicFormData(Map(formComponentId -> VariadicValue.One(value)))

  def ones(idAndValue: (FormComponentId, String)*): VariadicFormData =
    idAndValue.toList.foldMap { case (id, value) => one(id, value) }

  def many(formComponentId: FormComponentId, value: Seq[String]): VariadicFormData =
    VariadicFormData(Map(formComponentId -> VariadicValue.Many(value)))

  def manys(idAndValue: (FormComponentId, Seq[String])*): VariadicFormData =
    idAndValue.toList.foldMap { case (id, value) => many(id, value) }

  implicit val monoid: Monoid[VariadicFormData] = new Monoid[VariadicFormData] {
    override def empty: VariadicFormData = VariadicFormData.empty

    override def combine(x: VariadicFormData, y: VariadicFormData): VariadicFormData = x ++ y
  }

  // The VariadicFormData instance returned contains ALL fields in the data map, even if
  // there is no corresponding FormComponentId in the template.
  // The only use of the FormTemplate is to determine which branch of VariadicValue each FormComponentId should use,
  // with the assumption that a value of any FormComponentId found in the data map that is not
  // in the template should be represented by a VariadicValue.One value.
  def buildFromMongoData(template: FormTemplate, data: Map[FormComponentId, String]): VariadicFormData =
    buildFromMongoData(listVariadicFormComponentIds(template), data)

  // The VariadicFormData instance returned contains ALL fields in the data map, even if
  // there is no corresponding FormComponentId in the given set of form components Ids.
  // The only use of formComponentsIds set is to determine which branch of VariadicValue each FormComponentId should use,
  // with the assumption that a value of any FormComponentId found in the data map that is not
  // in the formComponentIds set should be represented by a VariadicValue.One value.
  def buildFromMongoData(
    variadicFormComponentIds: Set[FormComponentId],
    data: Map[FormComponentId, String]): VariadicFormData =
    VariadicFormData(
      data.map {
        case (id, s) =>
          if (variadicFormComponentIds(id.reduceToTemplateFieldId))
            (id, VariadicValue.Many(s.split(",").map(_.trim).filterNot(_.isEmpty).toSeq))
          else (id, VariadicValue.One(s))
      }
    )

  def listVariadicFormComponentIds(template: FormTemplate): Set[FormComponentId] =
    template.listAllSections.flatMap(listVariadicFormComponentIds).toSet

  def listVariadicFormComponentIds(section: BaseSection): Set[FormComponentId] =
    section.fields.flatMap(listVariadicFormComponentIds).toSet

  def listVariadicFormComponentIds(component: FormComponent): Set[FormComponentId] =
    component.`type` match {
      case g: Group  => listVariadicFormComponentIds(g.fields)
      case c: Choice => Set(component.id.reduceToTemplateFieldId)
      case r: RevealingChoice =>
        listVariadicFormComponentIds(r.options.toList.flatMap(_.revealingFields)) + component.id.reduceToTemplateFieldId
      case _ => Set.empty
    }

  def listVariadicFormComponentIds(components: List[FormComponent]): Set[FormComponentId] =
    components.flatMap(listVariadicFormComponentIds).toSet
}
