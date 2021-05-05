package com.phasmidsoftware.decisiontree.moves

/**
 * Type class for a State.
 *
 * @tparam S the type on which the state is based.
 *           For example, S might be Tic-tac-toe or Chess.
 */
trait State[S] extends Ordering[S] {

  /**
   * Method to determine if state s is a valid state.
   *
   * @param s a state.
   * @return a Boolean.
   */
  def isValid(s: S): Boolean

  /**
   * Method to determine if state s is a goal state.
   *
   * @param s a (current) state.
   * @return an Option of Boolean: if None then this state is not a goal state.
   *         If Some(b) then we got a result and the winner is the antagonist who moves first.
   */
  def isGoal(s: S): Option[Boolean]

  /**
   * Method to determine an estimate of a state's efficacy in reaching a goal..
   *
   * @param s a state.
   * @return a Double
   *         (in a domain appropriate to the type S where a higher value is always believed to be closer to a goal).
   */
  def heuristic(s: S): Double

  /**
   * Method to determine the possible moves from the given state.
   *
   * @param s a state.
   * @return a sequence of Transition[S]
   */
  def moves(s: S): Seq[Transition[S]]

  /**
   * Method to determine the ordering of two States.
   * It is based on the heuristic.
   *
   * @param x first state.
   * @param y second state.
   * @return <0 if x < y, >0 if x > y, else 0.
   */
  def compare(x: S, y: S): Int = heuristic(x).compare(heuristic(y))
}

/**
 * A function which transitions from a state S to a different state S.
 *
 * @tparam S the type of the input parameter and of the result.
 */
trait Transition[S] extends (S => S) {

  /**
   * Method to compose this Move with a Transition g.
   *
   * @param g a Transition[S].
   * @return a Transition[S].
   */
  def andThen(g: Transition[S]): Transition[S]
}

/**
 * A case class which implements Transition[S].
 *
 * @param f    a function S => S.
 * @param desc the human-legible description of f.
 * @tparam S the type of the input parameter and of the result.
 */
case class Move[S](f: S => S, desc: String) extends Transition[S] {
  /**
   * Apply this Move to s.
   *
   * @param s a state.
   * @return a new state.
   */
  override def apply(s: S): S = f(s)

  override def toString: String = desc

  /**
   * Composition of the descriptions of this and g.
   *
   * @param g a Transition[S] to be applied.
   * @return a human-legible rendering of f andThen g.
   */
  private def composedDesc(g: Transition[S]): String = {
    if (f == identity[S] _) g.toString
    else if (g == identity[S] _) desc
    else s"$g($desc)"
  }

  /**
   * Method to compose this Move with a Transition g.
   *
   * @param g a Transition[S].
   * @return a Transition[S].
   */
  def andThen(g: Transition[S]): Transition[S] = Move(f andThen g, composedDesc(g))
}

object Move {
  def identity[S]: Move[S] = Move(Predef.identity, "")
}

/**
 * LazyState is a domain-specific class which extends Transition by defining a function parameter and a human-legible description.
 *
 * @param state      the starting state.
 * @param transition a transition function which turns one S into another S (defaults to identity).
 * @tparam S type which defines the domain of this LazyState (must provide implicit evidence of State[T]).
 */
case class LazyState[S: State](state: S, transition: Transition[S] = Move.identity[S]) extends (() => S) {
  override def apply(): S = transition(state)
}

object LazyState