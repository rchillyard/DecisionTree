package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.decisiontree.examples.tictactoe.{TicTacToeInt, TicTacToeSlow}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EvaluatorSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Evaluator"

  it should "evaluate TicTacToeSlow" in {
    val eval = new Evaluator[TicTacToeSlow]
    val start: TicTacToeSlow = TicTacToeSlow()
    val so: Option[TicTacToeSlow] = eval.evaluate(start)
    so should matchPattern { case Some(_) => }
    val s = so.get
    //    println(s)
    // XXX ensure that X is the winner of this game.
    implicitly[State[TicTacToeSlow]].isGoal(s) shouldBe Some(true)
  }

  it should "evaluate TicTacToeInt" in {
    val eval = new Evaluator[TicTacToeInt]
    val start: TicTacToeInt = TicTacToeInt()
    val so: Option[TicTacToeInt] = eval.evaluate(start)
    so should matchPattern { case Some(_) => }
    val s = so.get
    //    println(s)
    // XXX ensure that X is the winner of this game.
    implicitly[State[TicTacToeInt]].isGoal(s) shouldBe Some(true)
  }

}
