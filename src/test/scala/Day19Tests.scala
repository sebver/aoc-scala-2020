import Day19._

class Day19Tests extends munit.FunSuite {
  test("and") {
    val ruleMap =
      Map(0 -> Rule.Sequence(0, List(1, 1, 2)), 1 -> Rule.Text(1, "a"), 2 -> Rule.Text(2, "b"))
    assertEquals(validate("aab", ruleMap), true)
  }

  test("or") {
    val ruleMap =
      Map(
        0 -> Rule.Or(0, Rule.Sequence(-1, List(1, 1, 2)), Rule.Sequence(-1, List(2, 2, 1))),
        1 -> Rule.Text(1, "a"),
        2 -> Rule.Text(2, "b")
      )
    assertEquals(validate("aab", ruleMap), true)
    assertEquals(validate("bba", ruleMap), true)
    assertEquals(validate("baa", ruleMap), false)
    assertEquals(validate("aba", ruleMap), false)
    assertEquals(validate("abb", ruleMap), false)
  }

  test("example") {

    val rules = """0: 4 1 5
                  |1: 2 3 | 3 2
                  |2: 4 4 | 5 5
                  |3: 4 5 | 5 4
                  |4: "a"
                  |5: "b"
                  |""".stripMargin.split("\n").toList

    val ruleMap = parse(rules)

    assertEquals(validate("ababbb", ruleMap), true)
    assertEquals(validate("bababa", ruleMap), false)
    assertEquals(validate("abbbab", ruleMap), true)
    assertEquals(validate("aaabbb", ruleMap), false)
    assertEquals(validate("aaaabbb", ruleMap), false)
  }

  test("example part 2") {
    val all = """42: 9 14 | 10 1
                |9: 14 27 | 1 26
                |10: 23 14 | 28 1
                |1: "a"
                |11: 42 31
                |5: 1 14 | 15 1
                |19: 14 1 | 14 14
                |12: 24 14 | 19 1
                |16: 15 1 | 14 14
                |31: 14 17 | 1 13
                |6: 14 14 | 1 14
                |2: 1 24 | 14 4
                |0: 8 11
                |13: 14 3 | 1 12
                |15: 1 | 14
                |17: 14 2 | 1 7
                |23: 25 1 | 22 14
                |28: 16 1
                |4: 1 1
                |20: 14 14 | 1 15
                |3: 5 14 | 16 1
                |27: 1 6 | 14 18
                |14: "b"
                |21: 14 1 | 1 14
                |25: 1 1 | 1 14
                |22: 14 14
                |8: 42
                |26: 14 22 | 1 20
                |18: 15 15
                |7: 14 5 | 1 21
                |24: 14 1""".stripMargin.split("\n").toList

    val lines   = """abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
                    |bbabbbbaabaabba
                    |babbbbaabbbbbabbbbbbaabaaabaaa
                    |aaabbbbbbaaaabaababaabababbabaaabbababababaaa
                    |bbbbbbbaaaabbbbaaabbabaaa
                    |bbbababbbbaaaaaaaabbababaaababaabab
                    |ababaaaaaabaaab
                    |ababaaaaabbbaba
                    |baabbaaaabbaaaababbaababb
                    |abbbbabbbbaaaababbbbbbaaaababb
                    |aaaaabbaabaaaaababaa
                    |aaaabbaaaabbaaa
                    |aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
                    |babaaabbbaaabaababbaabababaaab
                    |aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba""".stripMargin.split("\n").toList
    val ruleMap = updateRulesWithLoops(parse(all))
    assertEquals(validate("bbabbbbaabaabba", ruleMap), true)
    assertEquals(validate("babbbbaabbbbbabbbbbbaabaaabaaa", ruleMap), true)
    assertEquals(lines.count(validate(_, ruleMap)), 12)
  }
}
