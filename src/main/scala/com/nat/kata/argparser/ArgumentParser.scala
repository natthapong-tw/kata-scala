package com.nat.kata.argparser

import scala.util.Try

/**
 * http://codingdojo.org/kata/Args/
 */
object ArgumentParser {

  trait SchemeMapper[A] {
    def parse(stringParams: String): A
  }

  def extractArguments[A](stringParams: String)(implicit mapper: SchemeMapper[A]): Either[String,A] =
    Try(mapper.parse(stringParams))
      .fold(ex => Left(ex.getMessage), a =>Right(a))

  sealed trait ArgumentScheme
  sealed trait ValuedScheme[A] extends ArgumentScheme {
    def name: String
    def rawParse(stringArgument: String): List[Either[String, A]] = {
      val schemeName = name
      stringArgument.split("-").toList
        .filter(_.startsWith(s"$schemeName "))
        .map(_.split(" ").toList.map(_.trim))
        .map {
          case `schemeName` :: value :: _ => toArgumentValue(value)
          case `schemeName` :: Nil => Left(s"Scheme $name needs value")
          case _ => Left("Error")
        }
    }
    def parse(stringArgument: String): List[A] =
      rawParse(stringArgument)
        .collect({ case Right(v) => v})

    def toArgumentValue(stringArgument: String): Either[String, A]
  }

  case class NonValuedScheme(name: String) extends ArgumentScheme {
    def isExists(stringArgument: String): Boolean =
      stringArgument.split(" ").toList
        .map(_.toLowerCase)
        .contains(s"-$name")
  }

  case class IntScheme(override val name: String) extends ValuedScheme[Int] {
    def toArgumentValue(stringValue: String): Either[String, Int] =
      Try(stringValue.toInt).fold(ex => Left(ex.getMessage), v => Right(v))
  }

  case class StringScheme(override val name: String) extends ValuedScheme[String] {
    def toArgumentValue(stringValue: String): Either[String, String] = Right(stringValue)
  }
}
