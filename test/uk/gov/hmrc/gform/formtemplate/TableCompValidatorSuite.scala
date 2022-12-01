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

package uk.gov.hmrc.gform.formtemplate

import munit.FunSuite
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.core.{ Invalid, Valid }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ IncludeIf, TableComp, TableValue, TableValueRow }

class TableCompValidatorSuite extends FunSuite {
  test("TableCompValidator.validateTableComp (1)") {
    val table = TableComp(
      List(
        toSmartString("Column 1")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Column 1"), None, Some(5), None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )
    val res = TableCompValidator.validateTableComp(table)
    assertEquals(res, Invalid("The number of header columns and row values do not match"))
  }

  test("TableCompValidator.validateTableComp (2)") {
    val table = TableComp(
      List(
        toSmartString("Column 1"),
        toSmartString("Column 2"),
        toSmartString("Column 3"),
        toSmartString("Column 4"),
        toSmartString("Column 5")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Column 1"), None, Some(5), None),
            TableValue(toSmartString("Column 2"), None, None, None),
            TableValue(toSmartString("Column 3"), None, None, None),
            TableValue(toSmartString("Column 4"), None, None, None),
            TableValue(toSmartString("Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )
    val res = TableCompValidator.validateTableComp(table)
    assertEquals(res, Invalid("The number of header columns and row values do not match"))
  }

  test("TableCompValidator.validateTableComp (3)") {
    val table = TableComp(
      List(
        toSmartString("Column 1"),
        toSmartString("Column 2"),
        toSmartString("Column 3"),
        toSmartString("Column 4"),
        toSmartString("Column 5")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Column 1"), None, None, None),
            TableValue(toSmartString("Column 2"), None, Some(5), None),
            TableValue(toSmartString("Column 3"), None, None, None),
            TableValue(toSmartString("Column 4"), None, None, None),
            TableValue(toSmartString("Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )
    val res = TableCompValidator.validateTableComp(table)
    assertEquals(res, Invalid("The number of header columns and row values do not match"))
  }

  test("TableCompValidator.validateTableComp (4)") {
    val table = TableComp(
      List(
        toSmartString("Column 1"),
        toSmartString("Column 2")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Column 1"), None, Some(5), None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )
    val res = TableCompValidator.validateTableComp(table)
    assertEquals(res, Invalid("The number of header columns and row values do not match"))
  }

  test("TableCompValidator.validateTableComp (5)") {
    val table = TableComp(
      List(
        toSmartString("Column 1"),
        toSmartString("Column 2")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Column 1"), None, None, None),
            TableValue(toSmartString("Column 2"), None, None, None),
            TableValue(toSmartString("Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )
    val res = TableCompValidator.validateTableComp(table)
    assertEquals(res, Invalid("The number of header columns and row values do not match"))
  }

  test("TableCompValidator.validateTableComp (6)") {
    val table = TableComp(
      List(
        toSmartString("Column 1"),
        toSmartString("Column 2")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Column 1"), None, Some(2), None),
            TableValue(toSmartString("Column 2"), None, Some(1), None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )
    val res = TableCompValidator.validateTableComp(table)
    assertEquals(res, Invalid("The number of header columns and row values do not match"))
  }

  test("TableCompValidator.validateTableComp (7)") {
    val table = TableComp(
      List(
        toSmartString("Column 1"),
        toSmartString("Column 2")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Column 1"), None, Some(2), None),
            TableValue(toSmartString("Column 2"), None, Some(0), None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )
    val res = TableCompValidator.validateTableComp(table)
    assertEquals(res, Invalid("Invalid colspan value 0. Colspan must be number greater than 0"))
  }

  test("TableCompValidator.validateTableComp (8)") {
    val table = TableComp(
      List(
        toSmartString("Column 1"),
        toSmartString("Column 2")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Column 1"), None, Some(2), None),
            TableValue(toSmartString("Column 2"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )
    val res = TableCompValidator.validateTableComp(table)
    assertEquals(res, Invalid("The number of header columns and row values do not match"))
  }

  test("TableCompValidator.validateTableComp (9)") {
    val table = TableComp(
      List(
        toSmartString("Column 1"),
        toSmartString("Column 2"),
        toSmartString("Column 3"),
        toSmartString("Column 4")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Column 1"), None, Some(2), None),
            TableValue(toSmartString("Column 2"), None, None, None),
            TableValue(toSmartString("Column 3"), None, Some(2), None),
            TableValue(toSmartString("Column 4"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )
    val res = TableCompValidator.validateTableComp(table)
    assertEquals(res, Invalid("The number of header columns and row values do not match"))
  }

  test("TableCompValidator.validateTableComp (10)") {
    val table = TableComp(
      List(
        toSmartString("Column 1")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1, Column 1"), None, None, Some(5))
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val res = TableCompValidator.validateTableComp(table)
    assertEquals(
      res,
      Invalid("The number of header columns and row values do not match (rowspans exceed number of rows)")
    )
  }

  test("TableCompValidator.validateTableComp (11)") {
    val table = TableComp(
      List(
        toSmartString("Column 1"),
        toSmartString("Column 2")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1, Column 1"), None, None, Some(3)),
            TableValue(toSmartString("Row 1, Column 2"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 2, Colummn 1"), None, None, None),
            TableValue(toSmartString("Row 2, Colummn 2"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 3, Colummn 1"), None, None, None),
            TableValue(toSmartString("Row 3, Colummn 2"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val res = TableCompValidator.validateTableComp(table)
    assertEquals(res, Invalid("The number of header columns and row values do not match"))
  }

  test("TableCompValidator.validateTableComp (12)") {
    val table = TableComp(
      List(
        toSmartString("Column 1"),
        toSmartString("Column 2")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1, Column 1"), None, None, Some(3)),
            TableValue(toSmartString("Row 1, Column 2"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 2, Colummn 1"), None, None, Some(2)),
            TableValue(toSmartString("Row 2, Colummn 2"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 3, Colummn 1"), None, None, None),
            TableValue(toSmartString("Row 3, Colummn 2"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val res = TableCompValidator.validateTableComp(table)

    assertEquals(res, Invalid("The number of header columns and row values do not match"))
  }

  test("TableCompValidator.validateTableComp (13)") {
    val table = TableComp(
      List(
        toSmartString("Column 1")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1, Column 1"), None, None, Some(0))
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val res = TableCompValidator.validateTableComp(table)
    assertEquals(res, Invalid("Invalid rowspan value 0. Rowspan must be number greater than 0"))
  }

  test("TableCompValidator.validateTableComp (14)") {
    val table = TableComp(
      List(
        toSmartString("Column 1"),
        toSmartString("Column 2")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1, Column 1"), None, None, Some(3)),
            TableValue(toSmartString("Row 1, Column 2"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 2, Colummn 2"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 3, Colummn 2"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val res = TableCompValidator.validateTableComp(table)
    assertEquals(res, Valid)
  }

  test("TableCompValidator.validateTableComp (15)") {
    val table = TableComp(
      List(
        toSmartString("Header 1"),
        toSmartString("Header 2"),
        toSmartString("Header 3")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1 Column 1"), None, None, Some(3)),
            TableValue(toSmartString("Row 1 Column 2"), None, None, None),
            TableValue(toSmartString("Row 1 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 2 Column 2"), None, None, Some(2)),
            TableValue(toSmartString("Row 2 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 3 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 4 Column 1"), None, None, Some(3)),
            TableValue(toSmartString("Row 4 Column 2"), None, None, None),
            TableValue(toSmartString("Row 4 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 5 Column 2"), None, None, None),
            TableValue(toSmartString("Row 5 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 6 Column 2"), None, None, None),
            TableValue(toSmartString("Row 6 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 7 Column 1"), None, Some(3), Some(5))
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val res = TableCompValidator.validateTableComp(table)

    assertEquals(
      res,
      Invalid("The number of header columns and row values do not match (rowspans exceed number of rows)")
    )
  }

  test("TableCompValidator.normaliseTableComp (1)") {
    val table = TableComp(
      List(
        toSmartString("Column 1")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1, Column 1"), None, None, Some(2))
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      List(
        toSmartString("Column 1")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1, Column 1"), None, None, Some(2))
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString(""), None, None, Some(-1))
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val res = TableCompValidator.normaliseTableComp(table)
    assertEquals(res, expectedTable)
  }

  test("TableCompValidator.normaliseTableComp (2)") {
    val table = TableComp(
      List(
        toSmartString("Column 1"),
        toSmartString("Column 2")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1, Column 1"), None, None, Some(2)),
            TableValue(toSmartString("Row 1, Column 2"), None, None, Some(3))
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 3, Column 1"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      List(
        toSmartString("Column 1"),
        toSmartString("Column 2")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1, Column 1"), None, None, Some(2)),
            TableValue(toSmartString("Row 1, Column 2"), None, None, Some(3))
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString(""), None, None, Some(-1)),
            TableValue(toSmartString(""), None, None, Some(-2))
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 3, Column 1"), None, None, None),
            TableValue(toSmartString(""), None, None, Some(-1))
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val res = TableCompValidator.normaliseTableComp(table)
    assertEquals(res, expectedTable)
  }

  test("TableCompValidator.normaliseTableComp (3)") {
    val table = TableComp(
      List(
        toSmartString("Column 1"),
        toSmartString("Column 2"),
        toSmartString("Column 3")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1, Column 1"), None, Some(3), Some(4))
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 5, Column 1"), None, None, None),
            TableValue(toSmartString("Row 5, Column 2"), None, None, None),
            TableValue(toSmartString("Row 5, Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      List(
        toSmartString("Column 1"),
        toSmartString("Column 2"),
        toSmartString("Column 3")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1, Column 1"), None, Some(3), Some(4)),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString(""), None, Some(3), Some(-3)),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString(""), None, Some(3), Some(-2)),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString(""), None, Some(3), Some(-1)),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 5, Column 1"), None, None, None),
            TableValue(toSmartString("Row 5, Column 2"), None, None, None),
            TableValue(toSmartString("Row 5, Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val res = TableCompValidator.normaliseTableComp(table)

    assertEquals(res, expectedTable)
  }

  test("TableCompValidator.normaliseTableComp (4)") {
    val table = TableComp(
      List(
        toSmartString("Column 1")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Column 1"), None, Some(5), None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      List(
        toSmartString("Column 1")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Column 1"), None, Some(5), None),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val res = TableCompValidator.normaliseTableComp(table)

    assertEquals(res, expectedTable)
  }

  test("TableCompValidator.normaliseTableComp (5)") {
    val table = TableComp(
      List(
        toSmartString("Column 1")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Column 1"), None, None, Some(5))
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      List(
        toSmartString("Column 1")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Column 1"), None, None, Some(5))
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val res = TableCompValidator.normaliseTableComp(table)

    assertEquals(res, expectedTable)
  }

  test("TableCompValidator.normaliseTableComp (6)") {
    val table = TableComp(
      List(
        toSmartString("Header 1"),
        toSmartString("Header 2"),
        toSmartString("Header 3"),
        toSmartString("Header 4"),
        toSmartString("Header 5"),
        toSmartString("Header 6")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1 Column 1"), None, Some(3), None),
            TableValue(toSmartString("Row 1 Column 4"), None, None, Some(2)),
            TableValue(toSmartString("Row 1 Column 5"), None, Some(2), None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 2 Column 1"), None, None, None),
            TableValue(toSmartString("Row 2 Column 2"), None, None, None),
            TableValue(toSmartString("Row 2 Column 3"), None, None, None),
            TableValue(toSmartString("Row 2 Column 5"), None, None, None),
            TableValue(toSmartString("Row 2 Column 6"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      List(
        toSmartString("Header 1"),
        toSmartString("Header 2"),
        toSmartString("Header 3"),
        toSmartString("Header 4"),
        toSmartString("Header 5"),
        toSmartString("Header 6")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1 Column 1"), None, Some(3), None),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString("Row 1 Column 4"), None, None, Some(2)),
            TableValue(toSmartString("Row 1 Column 5"), None, Some(2), None),
            TableValue(toSmartString(""), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 2 Column 1"), None, None, None),
            TableValue(toSmartString("Row 2 Column 2"), None, None, None),
            TableValue(toSmartString("Row 2 Column 3"), None, None, None),
            TableValue(toSmartString(""), None, None, Some(-1)),
            TableValue(toSmartString("Row 2 Column 5"), None, None, None),
            TableValue(toSmartString("Row 2 Column 6"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val res = TableCompValidator.normaliseTableComp(table)

    assertEquals(res, expectedTable)
  }

  test("TableCompValidator.normaliseTableComp (7)") {
    val table = TableComp(
      List(
        toSmartString("Header 1"),
        toSmartString("Header 2"),
        toSmartString("Header 3"),
        toSmartString("Header 4"),
        toSmartString("Header 5")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1 Column 1"), None, None, None),
            TableValue(toSmartString("Row 1 Column 2"), None, None, None),
            TableValue(toSmartString("Row 1 Column 3"), None, None, None),
            TableValue(toSmartString("Row 1 Column 4"), None, None, None),
            TableValue(toSmartString("Row 1 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 2 Column 1"), None, None, None),
            TableValue(toSmartString("Row 2 Column 2"), None, Some(3), Some(3)),
            TableValue(toSmartString("Row 2 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 3 Column 1"), None, None, None),
            TableValue(toSmartString("Row 3 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 4 Column 1"), None, None, None),
            TableValue(toSmartString("Row 4 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 5 Column 1"), None, None, None),
            TableValue(toSmartString("Row 5 Column 2"), None, None, None),
            TableValue(toSmartString("Row 5 Column 3"), None, None, None),
            TableValue(toSmartString("Row 5 Column 5"), None, None, None),
            TableValue(toSmartString("Row 5 Column 6"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      List(
        toSmartString("Header 1"),
        toSmartString("Header 2"),
        toSmartString("Header 3"),
        toSmartString("Header 4"),
        toSmartString("Header 5")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1 Column 1"), None, None, None),
            TableValue(toSmartString("Row 1 Column 2"), None, None, None),
            TableValue(toSmartString("Row 1 Column 3"), None, None, None),
            TableValue(toSmartString("Row 1 Column 4"), None, None, None),
            TableValue(toSmartString("Row 1 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 2 Column 1"), None, None, None),
            TableValue(toSmartString("Row 2 Column 2"), None, Some(3), Some(3)),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString("Row 2 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 3 Column 1"), None, None, None),
            TableValue(toSmartString(""), None, Some(3), Some(-2)),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString("Row 3 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 4 Column 1"), None, None, None),
            TableValue(toSmartString(""), None, Some(3), Some(-1)),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString("Row 4 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 5 Column 1"), None, None, None),
            TableValue(toSmartString("Row 5 Column 2"), None, None, None),
            TableValue(toSmartString("Row 5 Column 3"), None, None, None),
            TableValue(toSmartString("Row 5 Column 5"), None, None, None),
            TableValue(toSmartString("Row 5 Column 6"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val res = TableCompValidator.normaliseTableComp(table)

    assertEquals(res, expectedTable)
  }

  test("TableCompValidator.normaliseTableComp (8)") {
    val table = TableComp(
      List(
        toSmartString("Header 1"),
        toSmartString("Header 2"),
        toSmartString("Header 3"),
        toSmartString("Header 4"),
        toSmartString("Header 5")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1 Column 1"), None, None, None),
            TableValue(toSmartString("Row 1 Column 2"), None, None, None),
            TableValue(toSmartString("Row 1 Column 3"), None, None, None),
            TableValue(toSmartString("Row 1 Column 4"), None, None, None),
            TableValue(toSmartString("Row 1 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 2 Column 1"), None, None, None),
            TableValue(toSmartString("Row 2 Column 2"), None, Some(4), Some(3)),
            TableValue(toSmartString("Row 2 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 3 Column 1"), None, None, None),
            TableValue(toSmartString("Row 3 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 4 Column 1"), None, None, None),
            TableValue(toSmartString("Row 4 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 5 Column 1"), None, None, None),
            TableValue(toSmartString("Row 5 Column 2"), None, None, None),
            TableValue(toSmartString("Row 5 Column 3"), None, None, None),
            TableValue(toSmartString("Row 5 Column 5"), None, None, None),
            TableValue(toSmartString("Row 5 Column 6"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      List(
        toSmartString("Header 1"),
        toSmartString("Header 2"),
        toSmartString("Header 3"),
        toSmartString("Header 4"),
        toSmartString("Header 5")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1 Column 1"), None, None, None),
            TableValue(toSmartString("Row 1 Column 2"), None, None, None),
            TableValue(toSmartString("Row 1 Column 3"), None, None, None),
            TableValue(toSmartString("Row 1 Column 4"), None, None, None),
            TableValue(toSmartString("Row 1 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 2 Column 1"), None, None, None),
            TableValue(toSmartString("Row 2 Column 2"), None, Some(4), Some(3)),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString("Row 2 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 3 Column 1"), None, None, None),
            TableValue(toSmartString(""), None, Some(4), Some(-2)),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString("Row 3 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 4 Column 1"), None, None, None),
            TableValue(toSmartString(""), None, Some(4), Some(-1)),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString("Row 4 Column 5"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 5 Column 1"), None, None, None),
            TableValue(toSmartString("Row 5 Column 2"), None, None, None),
            TableValue(toSmartString("Row 5 Column 3"), None, None, None),
            TableValue(toSmartString("Row 5 Column 5"), None, None, None),
            TableValue(toSmartString("Row 5 Column 6"), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val res = TableCompValidator.normaliseTableComp(table)

    assertEquals(res, expectedTable)
  }

  test("TableCompValidator.normaliseTableComp (9)") {
    val table = TableComp(
      List(
        toSmartString("Header 1"),
        toSmartString("Header 2"),
        toSmartString("Header 3")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1 Column 1"), None, None, Some(3)),
            TableValue(toSmartString("Row 1 Column 2"), None, None, None),
            TableValue(toSmartString("Row 1 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 2 Column 2"), None, None, Some(2)),
            TableValue(toSmartString("Row 2 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 3 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 4 Column 1"), None, None, Some(3)),
            TableValue(toSmartString("Row 4 Column 2"), None, None, None),
            TableValue(toSmartString("Row 4 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 5 Column 2"), None, None, None),
            TableValue(toSmartString("Row 5 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 6 Column 2"), None, None, None),
            TableValue(toSmartString("Row 6 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 7 Column 1"), None, Some(3), Some(5))
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      List(
        toSmartString("Header 1"),
        toSmartString("Header 2"),
        toSmartString("Header 3")
      ),
      List(
        TableValueRow(
          List(
            TableValue(toSmartString("Row 1 Column 1"), None, None, Some(3)),
            TableValue(toSmartString("Row 1 Column 2"), None, None, None),
            TableValue(toSmartString("Row 1 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString(""), None, None, Some(-2)),
            TableValue(toSmartString("Row 2 Column 2"), None, None, Some(2)),
            TableValue(toSmartString("Row 2 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString(""), None, None, Some(-1)),
            TableValue(toSmartString(""), None, None, Some(-1)),
            TableValue(toSmartString("Row 3 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 4 Column 1"), None, None, Some(3)),
            TableValue(toSmartString("Row 4 Column 2"), None, None, None),
            TableValue(toSmartString("Row 4 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString(""), None, None, Some(-2)),
            TableValue(toSmartString("Row 5 Column 2"), None, None, None),
            TableValue(toSmartString("Row 5 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString(""), None, None, Some(-1)),
            TableValue(toSmartString("Row 6 Column 2"), None, None, None),
            TableValue(toSmartString("Row 6 Column 3"), None, None, None)
          ),
          Option.empty[IncludeIf]
        ),
        TableValueRow(
          List(
            TableValue(toSmartString("Row 7 Column 1"), None, Some(3), Some(5)),
            TableValue(toSmartString(""), None, None, None),
            TableValue(toSmartString(""), None, None, None)
          ),
          Option.empty[IncludeIf]
        )
      ),
      toSmartString("Summary value")
    )

    val res = TableCompValidator.normaliseTableComp(table)

    assertEquals(res, expectedTable)
  }
}
