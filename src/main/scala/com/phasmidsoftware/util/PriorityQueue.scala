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

    def delOption: Option[(PriorityQueue[X], X)] = {
        val z = pq.del()
        for (v <- Option(z.getValue)) yield (PriorityQueue(z.getPq), v)
    }

    def del: (PriorityQueue[X], X) = {
        val z = pq.del()
        (PriorityQueue(z.getPq), z.getValue)
    }

    override def size: Int = pq.size()

    def iterator: Iterator[X] = for (x <- pq.iterator().asScala) yield x
}

object PriorityQueue {
    def minPQ[X: Ordering]: PriorityQueue[X] = new PriorityQueue(new PriorityQueueJava[X](implicitly[Ordering[X]]))

    def minPQ[X: Ordering](x: X): PriorityQueue[X] = new PriorityQueue(new PriorityQueueJava[X](x, implicitly[Ordering[X]]))

    def maxPQ[X: Ordering]: PriorityQueue[X] = new PriorityQueue(new PriorityQueueJava[X](implicitly[Ordering[X]].reverse))

    def maxPQ[X: Ordering](x: X): PriorityQueue[X] = new PriorityQueue(new PriorityQueueJava[X](x, implicitly[Ordering[X]].reverse))
}