package com.nat.kata.argparser

import org.scalatest.{FreeSpec, Matchers}

class ArgumentParserSpec extends FreeSpec with Matchers {

  import ArgumentParser._

  "extractArguments" - {

    "returns extracted specified value object" in {

      case class AppParams(
        logging: Boolean,
        port: Int,
        directory: String
      )

      val expectedAppParams = AppParams(true, 8080, "/user/logs")

      implicit object AppParamsSchemeMapper extends SchemeMapper[AppParams] {
        val default = AppParams(false, 80, "~")
        val schemes: List[ArgumentScheme] = List(
          NonValuedScheme("l"),
          IntScheme("p"),
          StringScheme("d")
        )
        def parse(stringParams: String): AppParams =
          schemes
          .foldLeft(default) { (prevConf, curr) =>
            curr match {
              case loggingScheme @ NonValuedScheme("l") if loggingScheme.isExists(stringParams) => prevConf.copy(logging = true)
              case portScheme @ IntScheme("p") => prevConf.copy(port = portScheme.parse(stringParams).headOption.getOrElse(prevConf.port))
              case stringScheme @ StringScheme("d") => stringScheme.parse(stringParams).headOption.map(v => prevConf.copy( directory = v)).getOrElse(prevConf)
              case _ => prevConf
            }
          }
      }

      ArgumentParser.extractArguments[AppParams]("-l -p 8080 -d /user/logs") shouldBe Right(expectedAppParams)
    }
  }

  "ArgumentScheme" - {

    "providing ArgsScheme should be able to detect specific scheme in the command input" - {

      "NonValuedScheme" - {

        "returns list of boolean values when found parameters" in {
          val inputParameters = "-l"
          NonValuedScheme("l").isExists(inputParameters) shouldBe true
        }

        "returns empty list when not found parameters" in {
          val inputParameters = "-d"
          NonValuedScheme("l").isExists(inputParameters) shouldBe false
        }

        "returns only matched parameters" in {
          val inputParameters = "-d -e -f -l"
          NonValuedScheme("l").isExists(inputParameters) shouldBe true
        }
      }

      "ValuedScheme" - {
        "IntScheme" - {
          "returns list of int values when found parameters" in {
            val inputParameters = "-p 8000"
            IntScheme("p").parse(inputParameters) shouldBe List(8000)
          }

          "returns more than 1 int values when specified more than 1 parameters" in {
            val inputParameters = "-p 8000 -p 9000"
            IntScheme("p").parse(inputParameters) shouldBe List(8000, 9000)
          }

          "returns only matched parameters" in {
            val inputParameters = "-p 8000 -p 9000 -d -e fg"
            IntScheme("p").parse(inputParameters) shouldBe List(8000, 9000)
          }
        }

        "StringScheme" - {
          "returns list of int values when found parameters" in {
            val inputParameters = "-d abcd"
            StringScheme("d").parse(inputParameters) shouldBe List("abcd")
          }

          "returns more than 1 int values when specified more than 1 parameters" in {
            val inputParameters = "-d defg -d bcda"
            StringScheme("d").parse(inputParameters) shouldBe List("defg", "bcda")
          }

          "returns only matched parameters" in {
            val inputParameters = "-d abcd -d defg -d -e fg"
            StringScheme("d").parse(inputParameters) shouldBe List("abcd", "defg")
          }
        }
      }

    }
  }
}
