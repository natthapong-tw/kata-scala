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

  }
}
