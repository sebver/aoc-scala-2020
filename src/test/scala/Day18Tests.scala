import Day18._

class Day18Tests extends munit.FunSuite {
  test("expressions normal") {
    assertEquals(result1("1 + 2 * 3 + 4 * 5 + 6"), 71L)
  }
  test("expressions parens") {
    assertEquals(result1("1 + (2 * 3) + (4 * (5 + 6))"), 51L)
    assertEquals(result1("2 * 3 + (4 * 5)"), 26L)
    assertEquals(result1("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"), 12240L)
    assertEquals(result1("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"), 13632L)
  }
  test("expressions normal part 2") {
    assertEquals(result2("1 + 2 * 3 + 4 * 5 + 6"), 231L)
  }

  test("expressions parens part 2".only) {
    assertEquals(result2("1 + (2 * 3) + (4 * (5 + 6))"), 51L)
    assertEquals(result2("2 * 3 + (4 * 5)"), 46L)
    assertEquals(result2("5 + (8 * 3 + 9 + 3 * 4 * 3)"), 1445L)
    assertEquals(result2("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"), 669060L)
    assertEquals(result2("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"), 23340L)
  }

  test("insertBracketsAroundAddition") {
    assertEquals(
      insertBracketsAroundAddition(List(Value(1L), Addition, Value(2L))),
      List(OpenBracket, Value(1L), Addition, Value(2), CloseBracket)
    )

    assertEquals(
      insertBracketsAroundAddition(List(Value(2), Multiplication, Value(1L), Addition, Value(2L))),
      List(Value(2L), Multiplication, OpenBracket, Value(1), Addition, Value(2), CloseBracket)
    )

    assertEquals(
      insertBracketsAroundAddition(
        List(
          Value(2),
          Multiplication,
          OpenBracket,
          Value(1L),
          Multiplication,
          Value(2L),
          CloseBracket,
          Addition,
          Value(3)
        )
      ),
      List(
        Value(2),
        Multiplication,
        OpenBracket,
        OpenBracket,
        Value(1L),
        Multiplication,
        Value(2L),
        CloseBracket,
        Addition,
        Value(3),
        CloseBracket
      )
    )

    assertEquals(
      insertBracketsAroundAddition(parse("1 + 2 * 3 + 4 * 5 + 6".replace(" ", ""))),
      List(
        OpenBracket,
        Value(1),
        Addition,
        Value(2),
        CloseBracket,
        Multiplication,
        OpenBracket,
        Value(3),
        Addition,
        Value(4),
        CloseBracket,
        Multiplication,
        OpenBracket,
        Value(5),
        Addition,
        Value(6),
        CloseBracket
      )
    )
  }
//  test("expreVssions normal part 2") {
//    assertEquals(result("1 + 2 * 3 + 4 * 5 + 6"), 71L)
//  }
}
