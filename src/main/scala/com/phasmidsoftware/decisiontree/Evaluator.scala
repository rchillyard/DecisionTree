package com.phasmidsoftware.decisiontree

import com.phasmidsoftware.decisiontree.moves.State
import com.phasmidsoftware.util.PriorityQueue

import scala.annotation.tailrec

class Evaluator[S: State] {

  def evaluate(s: S): Seq[S] = {
    @tailrec
    def inner(result: Seq[S], queue: PriorityQueue[S]): Seq[S] = if (queue.isEmpty) result else {
      val ss = implicitly[State[S]]
      val (q, s) = queue.del
      if (ss.isGoal(s))
        s +: result
      else {
        val qs = for (z <- ss.moves(s); q = z(s) if ss.isValid(q)) yield q
        inner(s +: result, qs.sorted.foldLeft(q)((y, s) => y.insert(s)))
      }
    }

    inner(Nil, PriorityQueue(s))
  }

  def result(ss: Seq[S]): (Option[Boolean], Seq[S]) = {
    val so = ss.headOption
    so match {
      case Some(_) => (Some(ss.size % 2 == 1), ss)
      case None => (None, ss)
    }
  }
}
