package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.util.{Loggable, Loggables, PriorityQueue}

import scala.annotation.tailrec

class Evaluator[S: State : Loggable] {

  def evaluate(s: S): Option[S] = {
    @tailrec
    def inner(queue: PriorityQueue[S]): Option[S] =
      if (queue.isEmpty)
        None
      else {
        //        "PQ size" !! queue.size
        val ss = implicitly[State[S]]
        val (q, s) = queue.del
        //        s"max value in priority queue is: $s" !! s
        if (ss.isGoal(s).isDefined)
          Some(s)
        else {
          val qs = for (z <- ss.moves(s); q = z(s) if ss.isValid(q)) yield q
          implicit val z: Loggable[Seq[S]] = new Loggables {}.seqLoggable[S]
          //          val sorted = "sorted moves" !! qs.sorted.reverse
          val sorted = qs.sorted.reverse
          inner(sorted.foldLeft(q)((y, s) => y.insert(s)))
        }
      }

    inner(PriorityQueue.maxPQ(s))
  }
}
