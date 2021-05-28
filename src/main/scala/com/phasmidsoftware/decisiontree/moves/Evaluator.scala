package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.flog.{Flog, Loggable}
import com.phasmidsoftware.util.PriorityQueue

import scala.annotation.tailrec

/**
 * Evaluate a game based on the State S.
 *
 * CONSIDER that with only one priority queue, the moves are going to get mixed up.
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
        def inner(queue: PriorityQueue[S]): Option[S] =
            if (queue.isEmpty)
                None
            else {
              val (q, s) = queue.del
              import flog._
              "max" !! s
              if (pSs.isGoal(s).isDefined)
                Some(s)
              else {
                val pq = PriorityQueue.maxPQ[S]
                val sorted = pSs.getStates(s, pq).sorted.reverse
                inner(sorted.foldLeft(q)((y, c) => y.insert("candidate" !! c)))
              }
            }

        inner(PriorityQueue.maxPQ(s))
    }
}
