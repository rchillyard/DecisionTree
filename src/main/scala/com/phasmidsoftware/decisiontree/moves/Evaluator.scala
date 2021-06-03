package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.flog.{Flog, Loggable}
import com.phasmidsoftware.util.PriorityQueue

import scala.annotation.tailrec

/**
 * Evaluate a game based on the State S.
 *
 * @tparam P the type of a parameter needed to construct a new S.
 * @tparam S the type of the game states, must support State.
 */
class Evaluator[P, S: Loggable](implicit pSs: State[P, S]) {
  val flog: Flog = Flog[Evaluator[P, S]]

  /**
   * Evaluate a game, starting with state s.
   *
   * @param s the starting state.
   * @return an Option[S]: if Some(s) then s is the first goal state to have been reached.
   *         if None then the queue of possibility was exhausted.
   */
  def evaluate(s: S): Option[S] = {
    @tailrec
    def inner(q1: PriorityQueue[S], q2: PriorityQueue[S]): Option[S] =
      if (q1.isEmpty)
        None
      else {
        val (q, s) = q1.del
        import flog._
        "max" !! s
        if (pSs.isGoal(s).isDefined) Some(s)
        else inner(q2.insertElements(pSs.getStates(s)), q)
      }

    inner(PriorityQueue.maxPQ(s), PriorityQueue.maxPQ)
  }
}
