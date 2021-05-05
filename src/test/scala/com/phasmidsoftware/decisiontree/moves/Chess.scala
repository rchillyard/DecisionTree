package com.phasmidsoftware.decisiontree.moves

/**
 * Mock class for testing Moves and States
 *
 * @param board a representation of the chess board.
 */
case class Chess(board: String)

object Chess {
  def checkmate(s: Chess): Option[Boolean] = s.board match {
    case "W checkmate" => Some(true)
    case "B checkmate" => Some(false)
    case _ => None
  }

  trait ChessState extends State[Chess] {
    def isValid(s: Chess): Boolean = true

    def heuristic(s: Chess): Double = 0

    def isGoal(s: Chess): Option[Boolean] = checkmate(s)

    def moves(s: Chess): Seq[Transition[Chess]] = Nil
  }

  implicit object ChessState extends ChessState
}
