package com.phasmidsoftware.util

import scala.jdk.CollectionConverters.IteratorHasAsScala

/**
 * This is a Min Priority Queue.
 *
 * @param pq Java priority queue.
 * @tparam X underlying type, must support Ordering.
 */
case class PriorityQueue[X: Ordering](pq: PriorityQueueJava[X]) extends Iterable[X] {

    def insert(x: X): PriorityQueue[X] = PriorityQueue(pq.insert(x))

    lazy val delOption: Option[(PriorityQueue[X], X)] = {
        val z = pq.del()
        for (v <- Option(z.getValue)) yield (PriorityQueue(z.getPq), v)
    }

    lazy val del: (PriorityQueue[X], X) = {
        val z = pq.del()
        (PriorityQueue(z.getPq), z.getValue)
    }

    override lazy val size: Int = pq.size()

    /**
     * Important NOTE: this PriorityQueue will be empty after the iterator has been traversed.
     * This is because, internally, a binary heap is not in any very particular order--
     * the sole invariant is "heap order."
     * Therefore, the only way to get the second smallest element is to delete the first smallest element.
     *
     * @return an Iterator.
     */
    lazy val iterator: Iterator[X] = for (x <- pq.iterator().asScala) yield x

    /**
     * Method to insert elements into this PriorityQueue.
     *
     * @param xs the elements to insert.
     * @return a new PriorityQueue.
     */
    def insertElements(xs: Seq[X]): PriorityQueue[X] = xs.foldLeft(this)((y, c) => y.insert(c))

    override def hashCode(): Int = pq.hashCode()

    override def equals(obj: Any): Boolean = obj match {
        case other: PriorityQueue[X] => pq.equals(other.pq)
        case _ => false
    }
}

object PriorityQueue {
    def apply[X: Ordering]: PriorityQueue[X] = PriorityQueue(new PriorityQueueJava[X](implicitly[Ordering[X]]))

    def apply[X: Ordering](x: X): PriorityQueue[X] = PriorityQueue(new PriorityQueueJava[X](x, implicitly[Ordering[X]]))

    def apply[X: Ordering](xs: Seq[X]): PriorityQueue[X] = {
        import scala.jdk.CollectionConverters._
        PriorityQueue(new PriorityQueueJava[X](xs.asJavaCollection, implicitly[Ordering[X]]))
    }

    def maxPQ[X: Ordering]: PriorityQueue[X] = apply(implicitly[Ordering[X]].reverse)

    def maxPQ[X: Ordering](x: X): PriorityQueue[X] = apply(x)(implicitly[Ordering[X]].reverse)

    def maxPQ[X: Ordering](xs: Seq[X]): PriorityQueue[X] = {
        import scala.jdk.CollectionConverters._
        PriorityQueue(new PriorityQueueJava[X](xs.asJavaCollection, implicitly[Ordering[X]].reverse))
    }
}