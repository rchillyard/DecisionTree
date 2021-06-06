package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.util.PriorityQueue

import scala.annotation.tailrec

/**
 * Trait to evaluate a state S according to some criterion.
 *
 * @tparam S the underlying type of the state to be evaluated.
 */
trait Evaluator[S] {
  /**
   * Evaluate a game, starting with state based on s.
   * If the starting state results in the achievement of a goal state, that state will be returned.
   * If the result is None, then no goal state could be achieved.
   *
   * @param s the starting state.
   * @return an Option[S]: if Some(s) then s is a goal state.
   *         if None then no goal was achieved.
   */
  def evaluate(s: S): Option[S]

  /**
   * Method to determine a sequence of S which can follow s, the given S.
   *
   * @param s the given S.
   * @return a sequence of S.
   */
  def states(s: S): Seq[S]
}


/**
 * Abstract implementation of Evaluator[S] which depends on an implicit value of State[P, S].
 *
 * @tparam P the type of the proto-state, i.e. a parameter needed to construct a new S.
 * @tparam S the underlying type of the state to be evaluated.
 */
abstract class Evaluator_State[P, S](implicit pSs: State[P, S]) extends Evaluator[S] {
  /**
   * Method to determine a sequence of S which can follow s, the given S.
   *
   * @param s the given S.
   * @return a sequence of S.
   */
  def states(s: S): Seq[S] = pSs.getStates(s)
}

/**
 * Implementation of Evaluator_State[P, S] which employs two priority queues (one for each player).
 *
 * @tparam P the type of the proto-state, i.e. a parameter needed to construct a new S.
 * @tparam S the underlying type of the state to be evaluated.
 */
class Evaluator_PQ[P, S](implicit pSs: State[P, S]) extends Evaluator_State[P, S] {

  /**
   * Evaluate a game, starting with state based on s.
   *
   * @param s the starting state.
   * @return an Option[S]: if Some(s) then s is a goal state.
   *         if None then no goal was achieved.
   */
  def evaluate(s: S): Option[S] = {
    @tailrec
    def inner(q1: PriorityQueue[S], q2: PriorityQueue[S]): Option[S] =
      if (q1.isEmpty)
        None
      else {
        val (q, s) = q1.del
        if (pSs.isGoal(s).isDefined) Some(s)
        else inner(q2.insertElements(states(s)), q)
      }

    inner(PriorityQueue.maxPQ(s), PriorityQueue.maxPQ)
  }
}
