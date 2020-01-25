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

  case class Friend(lastName: String, firstName: String, birthday: String, email: String)
}
