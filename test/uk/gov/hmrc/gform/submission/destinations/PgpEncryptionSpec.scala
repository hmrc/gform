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

package uk.gov.hmrc.gform.submission.destinations

import uk.gov.hmrc.gform.Spec

class PgpEncryptionSpec extends Spec {
  "createEncryptedData" should "load the correct public key and encrypt the input" in {
    val input: Array[Byte] = "test".getBytes
    val keyString = """-----BEGIN PGP PUBLIC KEY BLOCK-----
                      |Version: BCPG v1.58
                      |
                      |mQENBF4d4acBCAC7LjbaStGwNf3QgIPsIY8ViA1pC1CBMxd4ThIKv6FR26ugwzmK
                      |g1bVaQAvWoXmEddS8kNkZZmQXFNS+y++tNM9fcl70AacCO9p0E/lsKtaCmBzVJ7z
                      |WpJtFTryCgq4uXr1p5LWm6kILrryitRVG9/xpldFCOUy++gi7BiSwV5h0SroKVgm
                      |h7aCYiE1dSDWPSVSs8W9F6zCBVj8WyIG0Fo+g4mUk8OWcLw4WjqszF0F2gFcXBzy
                      |/Rb9gwUJNx/59p0BKWwuaHAc5okG4P6cHfLG6a2tTAYg4SYUQEpnovx8d4OYfmq8
                      |GZkE6KiEM5FM0QPmgmmfCJK0MD9zkRJp3ugLABEBAAG0JlNERVMtSU5URVJOQUwg
                      |PFNERVMtSU5URVJOQUxAc2Rlcy5jb20+iQEcBBABCgAGBQJeHeGnAAoJEJRngiqx
                      |h7hQv6AH/R/myUJK5uOVcHF3KhxakMMMxYvaDxsZ+GXz5UyrNs1RtdZmIhzIqIqw
                      |bKCNPkelQ6c7xzzMLo+k6v3rCwRywgJqguaUWA2Og0NrnDaXgIxXTMTY5663wwHd
                      |HMnvpFv5ouQt0h7T8/hVoDllZLR9fldch/PPrFH7G8oYPcHx7qNs0hEl+jCIZ06g
                      |WHqjA8UHu2B3uaOZKZB+FtjBFomncsbxS2BHL8WhFxk6StAWq3FnWWKWwBM4WLTg
                      |9VjzmQQuGchm2KFTcIzhPhjSD8RvC4yz81DQG03xvenjeskidPRa8IJqT2F56sVL
                      |4NAhEQy51I3pyKDwTn8UFgSWDsv+Ijw=
                      |=w56m
                      |-----END PGP PUBLIC KEY BLOCK-----""".stripMargin

    val encryptedData = PgpEncryption.createEncryptedData(keyString, input)

    encryptedData.length shouldBe 334

  }
}
