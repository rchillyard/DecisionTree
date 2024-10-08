package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.decisiontree.examples.tictactoe.{Board, TicTacToe}
import com.phasmidsoftware.flog.Loggable

object EvaluatorApp extends App {
  implicit val z: Loggable[TicTacToe] = (t: TicTacToe) => t.render()
  val eval = new Evaluator_PQ[Board, TicTacToe]()
  val start: TicTacToe = TicTacToe()
  val so: Option[TicTacToe] = eval.evaluate(start)
  // NOTE: we should always end with a draw, or a win by X. Never a win by 0.
  println(so.get.history)
}