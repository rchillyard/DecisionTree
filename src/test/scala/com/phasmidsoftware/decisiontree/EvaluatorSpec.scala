package com.phasmidsoftware.decisiontree

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EvaluatorSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Evaluator"

  it should "evaluate" in {
    val eval = new Evaluator[TicTacToe]
    val start: TicTacToe = TicTacToe()
    val states: Seq[TicTacToe] = eval.evaluate(start)
    println(s"${states.size} States in reverse order")
    println(states.mkString("---\n", "---\n", "---\n"))
  }

  it should "result" in {
    val eval = new Evaluator[TicTacToe]
    val start: TicTacToe = TicTacToe()
    val states: Seq[TicTacToe] = eval.evaluate(start)
    val (xo, _) = eval.result(states)
    xo should matchPattern { case Some(true) => }
  }

}
