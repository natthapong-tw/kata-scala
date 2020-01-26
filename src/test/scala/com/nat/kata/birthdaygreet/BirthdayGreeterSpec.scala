package com.nat.kata.birthdaygreet

import org.scalatest.{FreeSpec, Matchers}

class BirthdayGreeterSpec extends FreeSpec with Matchers {

  import BirthdayGreeter._
  "parseFile" - {

    "should parse correctly with correct input" in {
      val fileContent =
        """last_name, first_name, date_of_birth, email
          |Doe, John, 1982/10/08, john.doe@foobar.com
          |Ann, Mary, 1975/09/11, mary.ann@foobar.com
          |""".stripMargin

      parse(fileContent) shouldBe List(
        Friend("Doe", "John", "1982/10/08", "john.doe@foobar.com"),
        Friend("Ann", "Mary", "1975/09/11", "mary.ann@foobar.com")
      )
    }

    "should ignore failed line" in {
      val fileContent =
        """last_name, first_name, date_of_birth, email
          |Doe, John, 1982/10/08, john.doe@foobar.com
          |Ann, Mary, 1975/09/11, mary.ann@foobar.com
          |Other, Mary, 1975/09/11
          |""".stripMargin

      parse(fileContent) shouldBe List(
        Friend("Doe", "John", "1982/10/08", "john.doe@foobar.com"),
        Friend("Ann", "Mary", "1975/09/11", "mary.ann@foobar.com")
      )
    }
  }

  "replaceString" - {

    "should return new replaced string with one of them match" in {
      val template = "hello my name is {name}"
      val replacement = Replacement("{name}", "john")

      replace(template, List(replacement)) shouldBe "hello my name is john"
    }

    "should return new replaced string even there is more than 1 replacements" in {
      val template = "hello my name is {name} {lastname}"
      val nameReplacement = Replacement("{name}", "john")
      val lastnameReplacement = Replacement("{lastname}", "doe")

      replace(template, List(nameReplacement, lastnameReplacement)) shouldBe "hello my name is john doe"
    }
  }
}
