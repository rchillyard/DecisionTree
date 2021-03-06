package com.phasmidsoftware.tree

import com.phasmidsoftware.decisiontree.tree.Tree
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.Queue

class TreeSpec extends FlatSpec with Matchers {

  behavior of "preOrder"
  it should "work for small tree" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(3)))))
    target.preOrder(_ => true) shouldBe Queue(1, 2, 3)
  }
  it should "work for larger tree" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(3), Tree(4))), Tree(5, Seq(Tree(6), Tree(7)))))
    target.preOrder() shouldBe Queue(1, 2, 3, 4, 5, 6, 7)
  }
  it should "work when pruning away even branches" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(3), Tree(4))), Tree(5, Seq(Tree(6), Tree(7)))))
    target.preOrder(x => x % 2 != 0) shouldBe Queue(1, 5, 7)
  }
  it should "work when pruning away odd branches" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(3), Tree(4))), Tree(5, Seq(Tree(6), Tree(7)))))
    target.preOrder(x => x % 2 == 0) shouldBe Queue.empty
  }
  it should "work for a very deep tree" in {
    val n = 1000000
    val target: Tree[Int] = Stream.from(1).take(n).foldLeft(Tree(0))((t, x) => Tree(x, Seq(t)))
    val preOrder = target.preOrder()
    preOrder.size shouldBe (n+1)
  }

  behavior of "inOrder"
  it should "work (1))" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(3)))))
    target.inOrder() shouldBe Queue(3, 2, 1)
  }
  it should "work (2)" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(3), Tree(4))), Tree(5, Seq(Tree(6), Tree(7)))))
    target.inOrder() shouldBe Queue(3, 2, 4, 1, 6, 5, 7)
  }
  it should "work when pruning away even branches" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(3), Tree(4))), Tree(5, Seq(Tree(6), Tree(7)))))
    target.inOrder(x => x % 2 != 0) shouldBe Queue(1, 5, 7)
  }
  it should "work when pruning away odd branches" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(3), Tree(4))), Tree(5, Seq(Tree(6), Tree(7)))))
    target.inOrder(x => x % 2 == 0) shouldBe Queue.empty
  }
  it should "work for a very deep tree" in {
    val n = 1000000
    val target: Tree[Int] = Stream.from(1).take(n).foldLeft(Tree(0))((t, x) => Tree(x, Seq(t)))
    val inOrder = target.inOrder()
    inOrder.size shouldBe (n+1)
  }

  behavior of "postOrder"
  it should "work (1))" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(3)))))
    target.postOrder() shouldBe Queue(3, 2, 1)
  }
  it should "work (2)" in {
    val target = Tree(45, Seq(Tree(25, Seq(Tree(15), Tree(35))), Tree(75)))
    target.postOrder() shouldBe Queue(15, 35, 25, 75, 45)
  }
  it should "work (3)" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(4), Tree(5))), Tree(3, Seq(Tree(6), Tree(7)))))
    target.postOrder() shouldBe Queue(4, 5, 2, 6, 7, 3, 1)
  }
  it should "work when pruning away even branches" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(3), Tree(4))), Tree(5, Seq(Tree(6), Tree(7)))))
    target.postOrder(x => x % 2 != 0) shouldBe Queue(7, 5, 1)
  }
  it should "work when pruning away odd branches" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(3), Tree(4))), Tree(5, Seq(Tree(6), Tree(7)))))
    target.postOrder(x => x % 2 == 0) shouldBe Queue.empty
  }
  // FIXME why does this test not work? The tree builds OK but the postOrder never completes.
  ignore should "work for a very deep tree" in {
    val n = 1000000
    val target: Tree[Int] = Stream.from(1).take(n).foldLeft(Tree(0))((t, x) => Tree(x, Seq(t)))
    val postOrder = target.postOrder()
    postOrder.size shouldBe (n + 1)
  }

  behavior of "BFS"
  it should "work (1))" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(3)))))
    target.bfs() shouldBe Queue(1, 2, 3)
  }
  it should "work (2)" in {
    val target = Tree(45, Seq(Tree(25, Seq(Tree(15), Tree(35))), Tree(75)))
    target.bfs() shouldBe Queue(45, 25, 75, 15, 35)
  }
  it should "work (3)" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(4), Tree(5))), Tree(3, Seq(Tree(6), Tree(7)))))
    target.bfs() shouldBe Queue(1, 2, 3, 4, 5, 6, 7)
  }
  it should "work when pruning away even branches" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(3), Tree(4))), Tree(5, Seq(Tree(6), Tree(7)))))
    target.bfs(x => x % 2 != 0) shouldBe Queue(1, 5, 7)
  }
  it should "work when pruning away odd branches" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(3), Tree(4))), Tree(5, Seq(Tree(6), Tree(7)))))
    target.bfs(x => x % 2 == 0) shouldBe Queue.empty
  }

  behavior of "map"
  it should "work" in {
    val target = Tree(1, Seq(Tree(2, Seq(Tree(3)))))
    target.map(_.toString) shouldBe Tree("1", Seq(Tree("2", Seq(Tree("3")))))
  }

//  behavior of "flatMap"
//  it should "work" in {
//    val target = Tree(1, Seq(Tree(2, Seq(Tree(3)))))
//    target.flatMap(Tree(_)) shouldBe Tree("1", Seq(Tree("2", Seq(Tree("3")))))
//  }
}
