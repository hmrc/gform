/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.testonly

import java.time.Instant

object UniqueStringGenerator {
  val base = 32
  // removing 4 ambiguous characters (1, l, 0, o)
  val alphabet = ('2' to '9') ++ ('a' to 'k') ++ ('m' to 'n') ++ ('p' to 'z')

  def encode(number: Long): String = {
    def encodeRecursive(num: Long, acc: String): String =
      if (num == 0) {
        if (acc.isEmpty) "a" else acc
      } else {
        val remainder = (num % base).toInt
        val nextChar = alphabet(remainder)
        encodeRecursive(num / base, s"$nextChar$acc")
      }
    encodeRecursive(number, "")
  }
  def generateUniqueString(): String = {
    val currentTimestamp = Instant.now.toEpochMilli

    // Subtract a fixed timestamp to reduce the number's size
    val fixedTimestamp = Instant.parse("2024-01-01T00:00:00Z").toEpochMilli

    val reducedTimestamp = (currentTimestamp - fixedTimestamp) / 1000
    encode(reducedTimestamp)
  }
}
