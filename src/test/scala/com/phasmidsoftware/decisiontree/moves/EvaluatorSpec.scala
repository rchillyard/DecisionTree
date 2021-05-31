package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.decisiontree.examples.tictactoe.{Board, TicTacToe}
import com.phasmidsoftware.flog.Loggable
import com.phasmidsoftware.util.PriorityQueue.maxPQ
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EvaluatorSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Evaluator"

  private val bTs: State[Board, TicTacToe] = implicitly[State[Board, TicTacToe]]

  // TODO reinstate test
  ignore should "evaluate TicTacToe" in {
    implicit val z: Loggable[TicTacToe] = (t: TicTacToe) => "\n" + t.render()
    val eval = new Evaluator[Board, TicTacToe]
    val start: TicTacToe = TicTacToe()
    val so: Option[TicTacToe] = eval.evaluate(start)
    so should matchPattern { case Some(_) => }
    val s = so.get
    println(s.history.mkString("", "\n------\n", ""))
    bTs.isGoal(s) shouldBe None // Should be a draw.
  }

  it should "getStates" in {
    val start: TicTacToe = TicTacToe()
    val pq = bTs.getStates(start, maxPQ)
    val (_, t) = pq.del
    bTs.heuristic(t) shouldBe 3
    println(t.history)
  }

}
