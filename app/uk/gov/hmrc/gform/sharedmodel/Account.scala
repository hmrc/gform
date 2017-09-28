package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json.{Json, OFormat, OWrites, Reads}

case class Account(
                    sortCode: String,
                    accountNumber: String
                  )

object Account {
  val basic: OFormat[Account] = Json.format[Account]

  val writes: OWrites[Account] = OWrites[Account] { o =>
    Json.obj("account" -> Json.obj("sortCode" -> s"${o.sortCode.replaceAll("-", "")}", "accountNumber" -> s"${o.accountNumber}"))
  }

  val reads: Reads[Account] = basic

  implicit val format: OFormat[Account] = OFormat[Account](reads, writes)
}

