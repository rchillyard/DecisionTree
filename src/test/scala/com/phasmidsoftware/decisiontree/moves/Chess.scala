package com.phasmidsoftware.decisiontree.moves

case class Chess(board: String)

object Chess {
  trait ChessState extends State[Chess] {
    def isValid(s: Chess): Boolean = true

    def heuristic(s: Chess): Double = 0

    def isGoal(s: Chess, t: Chess): Boolean = false

    def moves(s: Chess): Seq[Move[Chess]] = Nil
  }

  implicit object ChessState extends ChessState
}

