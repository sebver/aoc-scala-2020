object Day4 extends Day(4) {

  def run2() = {

    val passports =
      readLines
        .mkString("\n")
        .split("\\n\\n")
        .map(_.replace("\n", " "))
        .map(
          _.split(" ")
            .map(_.trim)
            .filter(_.nonEmpty)
            .map {
              str =>
                val kvp = str.split(":")
                (kvp.head, kvp.drop(1).head)
            }
            .toMap
        )

    val required = Set("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt")

    passports.count {
      s =>
        required.forall(s.contains) &&
        yearRange(s("byr"), 1920, 2020) &&
        yearRange(s("iyr"), 2010, 2020) &&
        yearRange(s("eyr"), 2020, 2030) &&
        lengthRange(s("hgt")) &&
        s("hcl").matches("#[a-f0-9]{6}") &&
        s("ecl").matches("amb|blu|brn|gry|grn|hzl|oth") &&
        s("pid").matches("\\d{9}")
    }
  }

  def lengthRange(input: String) =
    input match {
      case x if x.matches("\\d*cm") =>
        val r = x.stripSuffix("cm").toInt
        r >= 150 && r <= 193
      case x if x.matches("\\d*in") =>
        val r = x.stripSuffix("in").toInt
        r >= 59 && r <= 76
      case _ =>
        false
    }

  def yearRange(input: String, min: Int, max: Int) =
    input.matches("\\d{4}") && (input.toInt match {
      case x if x >= min && x <= max => true
      case _                         => false
    })

}
