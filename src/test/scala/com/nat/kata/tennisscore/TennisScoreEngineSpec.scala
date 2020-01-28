package com.nat.kata.tennisscore

import com.nat.kata.tennisscore.TennisScoreEngine.{Deuce, ScoringState}
import org.scalatest.{FreeSpec, Matchers}

class TennisScoreEngineSpec extends FreeSpec with Matchers {

  import TennisScoreEngine._

  "ScoringState" - {

    "should deuce when score is (2, 3) and A scores" in {
      ScoringState(2, 3).AScore shouldBe Deuce
    }

    "should deuce when score is (3, 2) and B scores" in {
      ScoringState(3, 2).BScore shouldBe Deuce
    }

    "should be (1, 0) when A scores at (0, 0)" in {
      ScoringState(0, 0).AScore shouldBe ScoringState(1, 0)
    }
  }

  "calculateScore" - {
    "A score from base state" in {
      val history = List("A")

      calculateScore(history) shouldBe ScoringState(1, 0)
    }

    "B score from base state" in {
      val history = List("B")

      calculateScore(history) shouldBe ScoringState(0, 1)
    }

    "B scores 2 times in a row from base state" in {
      val history = List("B", "B")

      calculateScore(history) shouldBe ScoringState(0, 2)
    }

    "B deuces when A has a lead" in {
      val history = List("A", "A", "A", "B", "B", "B")

      calculateScore(history) shouldBe Deuce
    }

    "B gets advantage after deuce" in {
      val history = List("A", "A", "A", "B", "B", "B", "B")

      calculateScore(history) shouldBe BAdvantage
    }

    "No actions in history" in {
      val history = Nil

      calculateScore(history) shouldBe ScoringState(0, 0)
    }

    "Invalid input in history" in {
      val history = List("C")

      calculateScore(history) shouldBe ScoringState(0, 0)
    }

    "A player wins after scoring 2 times consecutively after deuce" in {
      val history = List("A", "A", "A", "B", "B", "B", "A", "A", "A")

      calculateScore(history) shouldBe AWin
    }

    "A player wins after scoring 1 times after deuce" in {
      val history = List("A", "A", "A", "B", "B", "B", "A", "A")

      calculateScore(history) shouldBe AAdvantage
    }
  }
}
