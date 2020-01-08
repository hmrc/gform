/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.submission

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent

// Contains the single FormField or MultiField FormFields for an 'atomic' FormComponent, depending on the FormComponent's
// `type`. 'Atomic' FormComponent types are those that are not containers for FormComponents, i.e. not Group or
// RevealingChoice
case class AtomicFormComponentFormFields(formComponent: FormComponent, fields: NonEmptyList[FormField])

case class SectionFormFieldsByAtomicFormComponents(
  sectionTitle: SmartString,
  fields: List[AtomicFormComponentFormFields])
