package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.util.PriorityQueue

import scala.annotation.tailrec

class Evaluator[S: State] {

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
          val qs = for (z <- ss.moves(s); q = z(s) if ss.isValid(q)) yield q
          val sorted = qs.sorted
          inner(sorted.foldLeft(q)((y, s) => y.insert(s)))
        }
      }

    inner(PriorityQueue.maxPQ(s))
  }
}
