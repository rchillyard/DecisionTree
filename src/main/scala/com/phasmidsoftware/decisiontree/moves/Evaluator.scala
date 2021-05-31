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
    def inner(queues: Pair[PriorityQueue[S]]): Option[S] =
        if (queues.head.isEmpty)
            None
        else {
            val (q, s) = queues.head.del
            import flog._
            "max" !! s
            if (pSs.isGoal(s).isDefined)
                Some(s)
            else {
                val pq = pSs.getStates(s, q)
                inner(queues.update(pq).swap(PriorityQueue.maxPQ))
            }
        }

      inner(Pair(PriorityQueue.maxPQ(s)))
  }
}

/**
 * Representation of a pair of objects which can be swapped.
 *
 * CONSIDER implementing with a T and an Option[T].
 *
 * @param t  the current "head" of the pair.
 * @param to the (optional) "tail" of the pair.
 * @tparam T the underlying type.
 */
case class Pair[T](t: T, to: Option[T]) {
    /**
     * Swap this Pair.
     *
     * @param z this is used in the event of this pair having only one T value.
     * @return a new Pair where the original t value is now the tail.
     */
    def swap(z: => T): Pair[T] = to match {
        case Some(x) => Pair(x, Some(t))
        case _ => Pair(z, Some(t))
    }

    /**
     * Update this Pair and return the updated value.
     *
     * @param z the value with which to replace the head value.
     * @return Pair(z, to).
     */
    def update(z: T): Pair[T] = copy(t = z)

    def head: T = t
}

object Pair {
    def apply[T](t: T): Pair[T] = Pair(t, None)
}

case class PairException(msg: String) extends Exception(msg)