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
        if (queues.t1.isEmpty)
            None
        else {
            val (q, s) = queues.t1.del
            import flog._
            "max" !! s
            if (pSs.isGoal(s).isDefined)
                Some(s)
            else {
                val ss: Seq[S] = pSs.getStates(s, q)
                val z: PriorityQueue[S] = ss.foldLeft(queues.t2)((y, c) => y.insert("candidate" !! c))
                val r: Pair[PriorityQueue[S]] = queues.swap.update(z)
                inner(r)
            }
        }

      inner(Pair(PriorityQueue.maxPQ(s), PriorityQueue.maxPQ))
  }
}

/**
 * Representation of a pair of objects which can be swapped.
 *
 * CONSIDER implementing with a T and an Option[T].
 *
 * @param t1 the current "head" of the pair.
 * @param t2 the (optional) "tail" of the pair.
 * @tparam T the underlying type.
 */
case class Pair[T](t1: T, t2: T) {

    /**
     * Swap the order of the members.
     *
     * @return a new Pair[T] with the members swapped.
     */
    def swap: Pair[T] = Pair(t2, t1)

    /**
     * Update this Pair and return the updated value.
     *
     * @param z the value with which to replace the head value.
     * @return Pair(z, to).
     */
    def update(z: T): Pair[T] = copy(t1 = z)

//    def head: T = t1
}

object Pair {
//    def apply[T](t: T): Pair[T] = Pair(t, None)
}

case class PairException(msg: String) extends Exception(msg)