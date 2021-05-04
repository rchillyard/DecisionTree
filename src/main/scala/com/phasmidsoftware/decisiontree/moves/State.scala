package com.phasmidsoftware.decisiontree.moves

/**
 * Type class for a State.
 *
 * @tparam S the type on which the state is based.
 *           For example, S might be Tic-tac-toe or Chess.
 */
trait State[S] {
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
   * @return a Boolean.
   */
  def isGoal(s: S): Boolean

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
   * @return a sequence of Move[S]
   */
  def moves(s: S): Seq[Transition[S]]
}

trait Transition[S] extends (S => S)

/**
 * Move is a domain-specific class which extends Transition by defining a function parameter and a human-legible description.
 *
 * @param transition  a transition function which turns one S into another S.
 * @param description a human-readable description for logging purposes.
 * @tparam S type which defines the domain of this Move (must provide implicit evidence of State[T]).
 */
case class Move[S: State](transition: S => S, description: String) extends Transition[S] {
  override def apply(s: S): S = transition(s)
}

object Move