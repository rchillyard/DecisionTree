package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.util.PriorityQueue
import scala.annotation.tailrec

/**
 * Evaluate a game based on the State S.
 *
 * CONSIDER that with only one priority queue, the moves are going to get mixed up.
 *
 * @tparam S the type of the game states, must support State.
 */
class Evaluator[S: State] {

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
                val ss = implicitly[State[S]]
                val (q, s) = queue.del
                if (ss.isGoal(s).isDefined)
                    Some(s)
                else {
                    val sorted = getStates(s).sorted.reverse
                    inner(sorted.foldLeft(q)((y, s) => y.insert(s)))
                }
            }

        inner(PriorityQueue.maxPQ(s))
    }

    /**
     * Get the possible states to follow the given state s.
     *
     * @param s the given state (of type S).
     * @return a sequence of S instances which are the possible states to follow s.
     */
    def getStates(s: S): Seq[S] = {
        val ss = implicitly[State[S]]
        for (z <- ss.moves(s); q = z(s) if ss.isValid(q)) yield q
    }
}
