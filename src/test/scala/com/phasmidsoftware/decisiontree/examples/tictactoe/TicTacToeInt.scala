package com.phasmidsoftware.decisiontree.examples.tictactoe

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe.stride
import com.phasmidsoftware.decisiontree.moves.{Move, State, Transition}
import com.phasmidsoftware.util.{DecisionTreeException, Shuffle}
import scala.util.{Failure, Success, Try}

case class TicTacToeInt(board: Int) {

  /**
   * Method to determine which player was responsible for creating this state.
   * The empty state will yield false (0 player).
   * A goal state will yield the winner.
   *
   * @return true for the first player (X), false for the second player (0)
   */
  lazy val player: Boolean = (stride * stride - open.size) % 2 == 1

  /**
   * Method to determine whether there is a line of marks.
   * The line may be horizontal (a row), vertical (a column) or diagonal.
   *
   * @return true if there is a line of similar marks in this TicTacToe.
   */
  lazy val line: Cell = rowDiagMatch(isLine) orElse transpose.rowDiagMatch(isLine)

  def row(i: Int): Int = TicTacToeIntOperations.row(board, i)

  /**
   * Method to determine whether there is a line of marks.
   * The line may be horizontal (a row), vertical (a column) or diagonal.
   *
   * @return true if there is a line of similar marks in this TicTacToe.
   */
  lazy val pendingLine: Cell = (rowDiagPendingMatch(isPendingLine) & transpose.rowDiagPendingMatch(isPendingLine)) flatMap {
    case 0 => Some(true)
    case 1 => Some(false)
    case _ => None
  }

  /**
   * Method to create a new TicTacToe from this TicTacToe.
   *
   * @param xOrO true if X is to play, false otherwise.
   * @param row  the row at which the mark should be made.
   * @param col  the column at which the mark should be made.
   * @return a new TicTacToe with the appropriate Cell marked.
   */
  def play(xOrO: Boolean)(row: Int, col: Int): TicTacToeInt = TicTacToeInt(TicTacToeIntOperations.play(board, xOrO, row, col))

  override def toString: String = TicTacToeIntOperations.render(board)

  /**
   * Val to determine the list of open cells from this TIcTacToe.
   *
   * @return a sequence of (Int, Int) tuples corresponding to the row, column indices.
   */
  lazy val open: Seq[(Int, Int)] = {
    val zs: Array[Int] = TicTacToeIntOperations.open(board)
    val q = for (z <- zs) yield z / stride -> z % stride
    q.toList // CONSIDER returning q as is.
  }

  val playX: (Int, Int) => TicTacToeInt = play(xOrO = true)
  val play0: (Int, Int) => TicTacToeInt = play(xOrO = false)


  private lazy val heuristic: Double = pendingLine match {
    case None => 0
    case Some(x) => if (x == player) 1 else -1
  }

  def transpose: TicTacToeInt = TicTacToeInt(TicTacToeIntOperations.transposeBoard(board))

  private def rowDiagMatch(f: Int => Cell): Cell = LazyList.from(0).take(3).map(row).map(f).foldLeft[Cell](None)((r, c) => r orElse c) orElse diagMatch(f)

  private def diagMatch(f: Int => Cell): Cell = f(TicTacToeIntOperations.diagonal(board))

  private def isLine(x: Int): Cell = TicTacToeIntOperations.rowLine(x) match {
    case 1 => Some(true)
    case 2 => Some(false)
    case _ => None
  }

  private def rowDiagPendingMatch(f: Int => Option[Int]): Option[Int] = LazyList.from(0).take(3).map(row).map(f).foldLeft[Option[Int]](None)((r, c) => r & c) & diagPendingMatch(f)

  private def isPendingLine(x: Int): Option[Int] = TicTacToeIntOperations.rowLinePending(TicTacToeIntOperations.row(board, x)) match {
    case 1 => Some(0)
    case 2 => Some(1)
    case _ => None
  }

  private def diagPendingMatch(f: Int => Option[Int]): Option[Int] = f(TicTacToeIntOperations.diagonal(board))
}

object TicTacToeInt {
  val stride = 3

  val start: TicTacToeInt = apply()

  def apply(): TicTacToeInt = apply(0)

  private def parseString(s: String): TicTacToeInt = {
    val chars = s.toCharArray.toSeq
    val cells: Seq[Int] = chars map {
      case ' ' | '.' => 0
      case 'X' => 1
      case '0' => 2
      case x => throw DecisionTreeException(s"TicTacToeInt: illegal character: $x")
    }
    if (cells.length < 9) throw DecisionTreeException("insufficient elements")
    else
      TicTacToeInt(TicTacToeIntOperations.parse(cells.toArray))
  }

  def parse(s: String): Try[TicTacToeInt] =
    if (s.length == stride * stride) Success(parseString(s))
    else Failure(DecisionTreeException(s"TicTacToeInt: parse failure: $s"))

  trait TicTacToeIntState extends State[TicTacToeInt] {
    /**
     * In this game, all states are valid.
     *
     * @param s a state.
     * @return true.
     */
    def isValid(s: TicTacToeInt): Boolean = true

    /**
     * How close are we to winning?
     *
     * @param s a state.
     * @return the number of our aligned cells - their aligned cells.
     */
    def heuristic(s: TicTacToeInt): Double = s.heuristic

    /**
     * Have we reached a result? And, if so, who won?
     *
     * @param s a (current) state.
     * @return an Option of Boolean: if None then this state is not a goal state.
     *         If Some(b) then we got a result and the winner is the antagonist who moves first.
     */
    def isGoal(s: TicTacToeInt): Option[Boolean] = s.line

    /**
     * Return all of the possible moves from the given state.
     *
     * @param s a state.
     * @return a sequence of Transition[S]
     */
    def moves(s: TicTacToeInt): Seq[Transition[TicTacToeInt]] = {
      val zs: Seq[(Int, Int)] = Shuffle(s.open, 3L) // we arbitrarily always want X to win
      val f: TicTacToeInt => (Int, Int) => TicTacToeInt = t => if (s.player) t.play0 else t.playX
      for (z <- zs) yield Move[TicTacToeInt](x => f(x)(z._1, z._2), z.toString())
    }
  }

  implicit object TicTacToeIntState extends TicTacToeIntState

}

