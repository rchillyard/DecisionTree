package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.flog.{Flog, Loggable}
import com.phasmidsoftware.util.PriorityQueue

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

  /**
   * Method to determine if a given S represents a "goal" state.
   *
   * @param s the given S.
   * @return None if s is not a goal;
   *         Some(false) if it is impossible to reach a goal state from s (typically a "draw");
   *         Some(true) if s is a final goal.
   */
  def isGoal(s: S): Option[Boolean] = pSs.isGoal(s)
}

/**
 * Implementation of Evaluator_State[P, S] which employs two priority queues (one for each player).
 *
 * @tparam P the type of the proto-state, i.e. a parameter needed to construct a new S.
 * @tparam S the underlying type of the state to be evaluated.
 */
// TODO remove Loggable
class Evaluator_PQ[P, S: Loggable](implicit pSs: State[P, S]) extends Evaluator_State[P, S] {

    // TODO remove logging stuff
    private val flog = Flog[Evaluator_PQ[P, S]]

    import flog._

    /**
     * Evaluate a game, starting with state based on s.
     *
     * @param s the starting state.
     * @return an Option[S]: if Some(s) then s is a goal state.
     *         if None then no goal was achieved.
     */
    def evaluate(s: S): Option[S] = {
        def updatePQ(pq: PriorityQueue[S], s: S) = pq.insertElements(states(s))

        // TODO restore tailrec
        //    @tailrec
        def inner(best: Option[S], q1: PriorityQueue[S], q2: PriorityQueue[S]): Option[S] =
            if (q1.isEmpty)
                "inner (empty PQ)" !! best
            else {
                val (q, s) = q1.del
                isGoal(s) match {
                    case Some(true) if pSs.isWin(s) =>
                        "inner isGoal: Some(true) returning" !! Some(s)
                    case Some(false) =>
                        // CONSIDER how do we know that Some(s) is "better" than best?
                        "inner isGoal: Some(false) returning recursive" !! inner(Some(s), updatePQ(q2, s), q)
                    case _ =>
                        "inner isGoal: None recursing" !! inner(best, updatePQ(q2, s), q)
                }
      }

    val result = inner(None, PriorityQueue.maxPQ(s), PriorityQueue.maxPQ)
    result
  }
}
