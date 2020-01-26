package com.nat.kata.birthdaygreet

object BirthdayGreeter {

  def parse(fileContent: String): List[Friend] =
    fileContent.split("\n")
    .toList.tail
    .map(_.split(",").map(_.trim).toList)
    .flatMap {
      case lastName :: firstName :: birthday :: email :: _ => Some(Friend(lastName, firstName, birthday, email))
      case _ => None
    }

  def replace(template: String, replacements: List[Replacement]): String =
    replacements.foldLeft(template) { (replaced, curr) =>
      replaced.replace(curr.matcher, curr.value)
    }

  case class Friend(lastName: String, firstName: String, birthday: String, email: String)

  case class Replacement(matcher: String, value: String)
}
