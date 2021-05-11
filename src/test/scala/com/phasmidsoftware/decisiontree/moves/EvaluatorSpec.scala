package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe
import com.phasmidsoftware.util.{Loggable, PriorityQueue}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EvaluatorSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Evaluator"

  it should "evaluate TicTacToe" in {
    implicit val z: Loggable[TicTacToe] = (t: TicTacToe) => "\n" + t.render()
    val eval = new Evaluator[TicTacToe]
    val start: TicTacToe = TicTacToe()
    val so: Option[TicTacToe] = eval.evaluate(start)
    so should matchPattern { case Some(_) => }
    val s = so.get
    println(s.history.mkString("", "\n------\n", ""))
    implicitly[State[TicTacToe]].isGoal(s) shouldBe Some(false) // Should be a draw.
  }

  it should "getStates" in {
    val eval = new Evaluator[TicTacToe]
    val start: TicTacToe = TicTacToe()
    val xs = eval.getStates(start).sorted.reverse
    implicitly[State[TicTacToe]].heuristic(xs.head) shouldBe 3
  }

  it should "use priorityQueue" in {
    val eval = new Evaluator[TicTacToe]
    val start: TicTacToe = TicTacToe()
    val ts = eval.getStates(start).sorted.reverse
    val q0: PriorityQueue[TicTacToe] = PriorityQueue.maxPQ
    val q1 = ts.foldLeft(q0)((y, s) => y.insert(s))
    val (_, t) = q1.del
    implicitly[State[TicTacToe]].heuristic(t) shouldBe 3
    println(t.history)
  }

}
