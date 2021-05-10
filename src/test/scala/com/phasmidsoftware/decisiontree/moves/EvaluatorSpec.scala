package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe
import com.phasmidsoftware.util.Loggable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EvaluatorSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Evaluator"

  ignore should "evaluate TicTacToe" in {
    implicit val z: Loggable[TicTacToe] = (t: TicTacToe) => "\n" + t.render()
    val eval = new Evaluator[TicTacToe]
    val start: TicTacToe = TicTacToe()
    val so: Option[TicTacToe] = eval.evaluate(start)
    so should matchPattern { case Some(_) => }
    val s = so.get
    println(s.history.mkString("", "\n------\n", ""))
    implicitly[State[TicTacToe]].isGoal(s) shouldBe Some(false) // Should be a draw.
  }

}
