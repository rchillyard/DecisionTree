package com.phasmidsoftware.decisiontree.tree

trait LazyNode[T] extends Node[T] {
  /**
   * Generator function.
   */
  val f: T => Seq[T]

  /**
   * Unit method to construct a new LazyMode based on t.
   *
   * @param t the t value with which to create a new lazyNode.
   * @return a LazyNode based on t.
   */
  def unit(t: T): LazyNode[T]

  /**
   * The children of this Node.
   */
  def children: Seq[Node[T]] = f(key).map(unit)
}

class LazyTree[T](t: T)(val f: T => Seq[T])(implicit goal: Goal[T]) extends LazyNode[T] {

  /**
   * Unit method to construct a new LazyMode based on t.
   *
   * @param t the T value from which to construct the new LazyTree.
   * @return a new LazyTree based on t.
   */
  override def unit(t: T): LazyNode[T] = new LazyTree(t)(f)

  /**
   * The key of this Node.
   */
  override val key: T = t
}

object LazyTree {
  def apply[T](t: T)(f: T => Seq[T])(implicit goal: Goal[T]) = new LazyTree(t)(f)
}