package uk.gov.hmrc.gform

import uk.gov.hmrc.gform.core.parsers.BooleanExprParser

class SimpleParseTest extends Spec {

  it should "parse simple expression" in {
    val res = BooleanExprParser.validate("${amountA=22}")
    res shouldBe('right)

  }


}
