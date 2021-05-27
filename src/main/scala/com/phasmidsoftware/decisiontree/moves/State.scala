package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.util.PriorityQueue

/**
 * Type class for a State.
 *
 * @tparam S the type on which the state is based.
 *           For example, S might be Tic-tac-toe or Chess.
 */
trait State[P, S] extends Ordering[S] {
  /**
   * Method to construct an S from the following parameters:
   *
   * CONSIDER refactoring this to take parameters (P,S) and PriorityQueue[S].
   *
   * @param proto a (P, Q).
   * @param q     a PriorityQueue.
   * @return an S.
   */
  def construct(proto: (P, S), q: PriorityQueue[S]): S

  /**
   * Method to yield the previous state.
   *
   * @param s a value of S for which we need the previous.
   * @return an optional S.
   */
  def previous(s: S): Option[S]

  /**
   * Yield the PriorityQueue which for this state.
   * Any instances of S which have already been removed from the PQ will of course not be present.
   *
   * @param s a value of S for which we need the priority queue.
   * @return a PriorityQueue[S].
   */
  def pq(s: S): PriorityQueue[S]

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
   *         If Some(true) then we got a win; otherwise, if Some(false) then
   *         we terminated without a win (a draw).
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
  def moves(s: S): Seq[Transition[P, S]]

  /**
   * Concrete method to get the possible states to follow the given state s.
   *
   * @param s the given state (of type S).
   * @return a sequence of S instances which are the possible states to follow s.
   */
  def getStates(s: S, pq: PriorityQueue[S]): Seq[S] =
    for (z <- moves(s); w = z(s); q = construct(w, pq) if isValid(q)) yield q

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
 * A function which transitions from a state S to a prototype state.
 *
 * @tparam P a proto-state, from which a state S can be constructed.
 * @tparam S the type of the input..
 */
trait Transition[P, S] extends (S => (P, S))

/**
 * A case class which implements Transition[S].
 *
 * @param f    a function S => S.
 * @param desc the human-legible description of f.
 * @tparam P a proto-state, from which a state S can be constructed.
 * @tparam S the type of the input parameter and of the result.
 */
case class Move[P, S](f: S => P, desc: String) extends Transition[P, S] {

  /**
   * Apply this Move to s.
   *
   * @param s a state.
   * @return a new proto-state.
   */
  override def apply(s: S): (P, S) = f(s) -> s

  override def toString: String = desc
}

/**
 * LazyState is a domain-specific class which extends Transition by defining a function parameter and a human-legible description.
 *
 * @param state      the starting state.
 * @param transition a transition function which turns one S into another S (defaults to identity).
 * @tparam S type which defines the domain of this LazyState (must provide implicit evidence of State[T]).
 */
case class LazyState[P, S](state: S, transition: Transition[P, S])(implicit pSs: State[P, S]) extends (() => P) {
  override def apply(): P = transition(state)._1
}

object LazyState