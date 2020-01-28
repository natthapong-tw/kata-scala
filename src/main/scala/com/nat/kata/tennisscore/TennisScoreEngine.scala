package com.nat.kata.tennisscore

object TennisScoreEngine {

  case class GameRules(noOfAdvantages: Int)

  sealed trait TennisScoreState {
    def AScore: TennisScoreState
    def BScore: TennisScoreState
  }
  case class ScoringState(aScore: Int, bScore: Int) extends TennisScoreState {
    def AScore: TennisScoreState =
      (aScore, bScore) match {
        case (2, 3) => Deuce
        case (a, _) if a > 3 => AWin
        case (_, _) => copy(aScore = aScore + 1)
      }

    def BScore: TennisScoreState =
      (aScore, bScore) match {
        case (3, 2) => Deuce
        case (_, b) if b > 3 => BWin
        case (_, _) => copy(bScore = bScore + 1)
      }
  }

  case object Deuce extends TennisScoreState {
    override def AScore: TennisScoreState = AAdvantage
    override def BScore: TennisScoreState = BAdvantage
  }

  case object AAdvantage extends TennisScoreState {
    override def AScore: TennisScoreState = AWin
    override def BScore: TennisScoreState = Deuce
  }
  case object BAdvantage extends TennisScoreState {
    override def AScore: TennisScoreState = Deuce
    override def BScore: TennisScoreState = BWin
  }

  case object AWin extends TennisScoreState {
    override def AScore: TennisScoreState = AWin
    override def BScore: TennisScoreState = AWin
  }
  case object BWin extends TennisScoreState {
    override def AScore: TennisScoreState = BWin
    override def BScore: TennisScoreState = BWin
  }


  def calculateScore(history: List[String]): TennisScoreState = {
    val baseState: TennisScoreState = ScoringState(0, 0)
    history.foldLeft(baseState) { (state, action) =>
      action match {
        case "A" => state.AScore
        case "B" => state.BScore
        case _ => state
      }
    }
  }
}
