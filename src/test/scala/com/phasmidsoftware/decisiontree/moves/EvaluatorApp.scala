package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe
import com.phasmidsoftware.flog.Loggable

object EvaluatorApp extends App {
  implicit val z: Loggable[TicTacToe] = (t: TicTacToe) => t.render()
  val eval = new Evaluator[TicTacToe]
  val start: TicTacToe = TicTacToe()
  val so: Option[TicTacToe] = eval.evaluate(start)
  println(so)
  println(so map (implicitly[State[TicTacToe]].isGoal(_)))
}