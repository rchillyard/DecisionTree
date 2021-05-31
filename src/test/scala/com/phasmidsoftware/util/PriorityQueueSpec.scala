package com.phasmidsoftware.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PriorityQueueSpec extends AnyFlatSpec with should.Matchers {

    behavior of "PriorityQueue"

    it should "headOption" in {
        PriorityQueue[Int].headOption should matchPattern { case None => }
        PriorityQueue[Int](1).headOption should matchPattern { case Some(_) => }
    }

    it should "insert" in {
        PriorityQueue[Int].insert(1) shouldBe PriorityQueue[Int](1)
    }

    it should "iterator" in {
        val iterator = PriorityQueue[Int](1).iterator
        iterator.hasNext shouldBe true
        iterator.next() shouldBe 1
        iterator.hasNext shouldBe false
    }

    it should "head" in {
        PriorityQueue[Int](1).insert(2).head shouldBe 1
    }

    it should "del" in {
        val (pq, x) = PriorityQueue(1).insert(2).del
        x shouldBe 1
        pq shouldBe PriorityQueue(2)
    }

    it should "apply1" in {
        PriorityQueue[Int].isEmpty shouldBe true
    }

    it should "apply2" in {
        PriorityQueue(1).isEmpty shouldBe false
    }

    it should "apply3" in {
        PriorityQueue(new PriorityQueueJava[Int]()).isEmpty shouldBe true
    }

    it should "maxPQ1" in {
        PriorityQueue.maxPQ[Int].insert(1).insert(2).head shouldBe 2
    }

    it should "maxPQ2" in {
        PriorityQueue.maxPQ[Int](1).insert(2).head shouldBe 2
    }

}
