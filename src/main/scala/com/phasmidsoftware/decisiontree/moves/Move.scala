package com.phasmidsoftware.decisiontree.moves

trait State[S] {
  def isValid(s: S): Boolean

  def heuristic(s: S): Double

  def isGoal(s: S, t: S): Boolean

  def moves(s: S): Seq[Move[S]]
}

case class Move[S: State](f: S => S, description: String) extends (S => S) {
  override def apply(s: S): S = f(s)
}

object Move