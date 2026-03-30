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
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object HandlebarsValidationHelpers {

  def extractTokensFromPayload(payload: String): Set[String] = {
    val pattern = """\{\{([^}]*)}}""".r
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
      // Replace special characters with space (to keep tokens separate)
      .map(_.replaceAll("[#/()';^=>*|]+", " "))
      // Split on whitespace to get individual tokens
      .flatMap(_.trim.split("\\s+"))
      // Exclude pure numeric tokens as they're commonly used as literals
      .filterNot(_.matches("^\\d+$"))
      // Exclude Handlebars block parameters (e.g., "@index")
      .filterNot(_.startsWith("@"))
      .toSet
  }

  def extractSyntheticTokens(payload: String): Set[String] = {
    val tokenPattern = "\\{\\{\\s*#with[^}|]+\\|([a-zA-Z0-9]+)\\|\\s*}}".r
    tokenPattern.findAllMatchIn(payload).map(_.group(1)).toList.filterNot(_ === ".").toSet
  }

  val knownHelpersAndReservedWords: Set[String] =
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
      // Reserved/special words
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
      "this",
      "periodKey",
      "periodFrom",
      "periodTo",
      "formStatus",
      "summaryHtml",
      "instructionHtml",
      "submissionReference",
      "caseworker",
      "uploadedFiles",
      "name",
      "extension",
      "data",
      "status",
      "submissionRef",
      "formBundle",
      "tree",
      "submissionRefs",
      "user",
      "enrolledIdentifier",
      "customerId"
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
}
