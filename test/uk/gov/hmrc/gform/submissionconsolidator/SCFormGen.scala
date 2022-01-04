/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.submissionconsolidator

import java.time.format.DateTimeFormatter
import java.time.{ Instant, ZoneOffset }

import org.scalacheck.Gen

trait SCFormGen {

  val DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")

  def genTimestampStr: Gen[String] =
    for {
      numSeconds <- Gen.choose(0, 10000)
    } yield Instant.now().minusSeconds(numSeconds.toLong).atOffset(ZoneOffset.UTC).format(DATE_TIME_FORMATTER)

  def genFormField: Gen[SCFormField] =
    for {
      id    <- Gen.alphaNumStr.suchThat(!_.isEmpty)
      value <- Gen.alphaNumStr.suchThat(!_.isEmpty)
    } yield SCFormField(id, value)

  def genForm: Gen[SCForm] =
    for {
      submissionRef       <- Gen.uuid.map(_.toString)
      projectId           <- Gen.alphaNumStr.suchThat(!_.isEmpty)
      templateId          <- Gen.alphaNumStr.suchThat(!_.isEmpty)
      customerId          <- Gen.alphaNumStr.suchThat(!_.isEmpty)
      submissionTimestamp <- genTimestampStr
      formData            <- Gen.listOf(genFormField)
    } yield SCForm(
      submissionRef,
      projectId,
      templateId,
      customerId,
      submissionTimestamp,
      formData
    )
}

object SCFormGen extends SCFormGen
