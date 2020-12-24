package com.phasmidsoftware.decisiontree.tree

import scala.collection.mutable.ListBuffer

/**
 * Type class to define a visitor for the nodes of a Tree.
 * It differs from Visitor in that V is expected to be mutable and, therefore, method visit yields Unit.
 * There is one method defined: visit.
 */
trait MutatingVisitor[T, V] {
  /**
   * Method to be called when visiting a Node[T] of a Tree[T].
   *
   * @param v the visitor, of type V.
   * @param t the key of the current Node.
   */
  def visit(v: V, t: T): Unit
}


/**
 * Companion object of MutatingVisitor.
 */
object MutatingVisitor {

  /**
   * This is the equivalent of StackVisitor.
   *
   * @tparam T the underlying type.
   */
  trait AppendingListVisitor[T] extends MutatingVisitor[T, ListBuffer[T]] {
    def visit(ts: ListBuffer[T], t: T): Unit = {
      ts append t
    }
  }

  implicit def appendingListVisitor[T]: MutatingVisitor[T, ListBuffer[T]] = new AppendingListVisitor[T] {}

  /**
   * This is the equivalent of StackVisitor.
   *
   * @tparam T the underlying type.
   */
  trait PrependingListVisitor[T] extends MutatingVisitor[T, ListBuffer[T]] {
    def visit(ts: ListBuffer[T], t: T): Unit = {
      ts prepend t
    }
  }

  implicit def prependingListVisitor[T]: MutatingVisitor[T, ListBuffer[T]] = new PrependingListVisitor[T] {}
}

