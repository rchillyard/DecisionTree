package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.decisiontree.examples.tictactoe.{Board, TicTacToe}
import com.phasmidsoftware.util.PriorityQueue
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
 * NOTE: this Spec file depends on TicTacToe.
 */
class EvaluatorSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Evaluator on TicTacToe"

  private val bTs: State[Board, TicTacToe] = implicitly[State[Board, TicTacToe]]

  // FIXME Issue_8 reinstate this test: it should end in a draw
  it should "evaluate TicTacToe" in {
    val eval: Evaluator[TicTacToe] = new Evaluator_PQ[Board, TicTacToe]
    val start: TicTacToe = TicTacToe()
    val so: Option[TicTacToe] = eval.evaluate(start)
    so should matchPattern { case None => }
  }

  it should "compare" in {
    val t: TicTacToe = bTs.construct(Board(1, 0x800000) -> TicTacToe())
    bTs.compare(t, t) shouldBe 0
  }

  it should "getStates" in {
    val start: TicTacToe = TicTacToe()
    val ts: Seq[TicTacToe] = bTs.getStates(start)
    val pq = PriorityQueue.maxPQ(ts)
    pq.isEmpty shouldBe false
    val (_, t) = pq.del
    bTs.heuristic(t) shouldBe 4
    println(t.history)
  }

}
