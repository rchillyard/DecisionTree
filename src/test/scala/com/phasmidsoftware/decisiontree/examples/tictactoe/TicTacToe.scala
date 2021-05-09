package com.phasmidsoftware.decisiontree.examples.tictactoe

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe.start
import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToeOps._
import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToeSlow.stride
import com.phasmidsoftware.decisiontree.moves.{Move, State, Transition}
import com.phasmidsoftware.util.{DecisionTreeException, Loggable, Loggables, Shuffle}
import scala.util.{Failure, Success, Try}

/**
 * This class represents 9 x 2 bits, at the high end of the 32-bit word.
 *
 * @param value the bit value of this row.
 */
case class Board(value: Int) extends AnyVal {
  def row(i: Int): Row = TicTacToeOps.row(value, i)

  def toHexString: String = value.toHexString

  def transpose: Board = Board(transposeBoard(value))
}

/**
 * Case class to represent a TicTacToe situation.
 *
 * @param board the current layout.
 * @param prior the prior situation.
 */
case class TicTacToe(board: Board, prior: TicTacToe = start) {

  /**
   * Defines a Matching type which takes a Row and returns a Cell.
   */
  type Matching = Row => Cell

  /**
   * Method to determine which player was responsible for creating this state.
   * The empty state will yield false (0 player).
   * A goal state will yield the winner.
   *
   * @return true for the first player (X), false for the second player (0)
   */
  lazy val player: Boolean = (stride * stride - open.size) % 2 == 1

  /**
   * Method to determine whether there is a line of marks for one player.
   * The line may be horizontal (a row), vertical (a column) or diagonal.
   *
   * @return Some(b) if there is a line of similar marks in this TicTacToe otherwise None.
   *         The Boolean b is true for player X, and false for player 0.
   */
  lazy val win: Cell = isWin(rowsR0) orElse isWin(rowsL0) orElse isWin(diagonals)

  /**
   * Method to determine whether there is a potential win.
   * The line may be horizontal (a row), vertical (a column) or diagonal and missing one element.
   *
   * @return true if there is a line of two similar marks with a space between in this TicTacToe.
   */
  lazy val peneWin: Cell = isPendingWin(rowsR0) orElse isPendingWin(rowsL0) orElse isPendingWin(diagonals)

  /**
   * Method to create a new TicTacToe from this TicTacToe.
   *
   * @param xOrO true if X is to play, false otherwise.
   * @param row  the row at which the mark should be made.
   * @param col  the column at which the mark should be made.
   * @return a new TicTacToe with the appropriate Cell marked.
   */
  def play(xOrO: Boolean)(row: Int, col: Int): TicTacToe = TicTacToe(Board(TicTacToeOps.play(board.value, xOrO, row, col)), this)

  def render: String = s"${TicTacToeOps.render(board.value)} ($heuristic)"

  lazy val history: List[String] = prior match {
    case TicTacToe(Board(0), _) => List(render)
    case x => x.history :+ render
  }

  override def toString: String = s"${board.toHexString}"

  /**
   * Val to determine the list of open cells from this TIcTacToe.
   *
   * @return a sequence of (Int, Int) tuples corresponding to the row, column indices.
   */
  lazy val open: Seq[(Int, Int)] = {
    val zs: Array[Int] = TicTacToeOps.open(board.value)
    val q = for (z <- zs) yield z / stride -> z % stride
    q.toList // CONSIDER returning q as is.
  }

  val playX: (Int, Int) => TicTacToe = play(xOrO = true)
  val play0: (Int, Int) => TicTacToe = play(xOrO = false)

  override def hashCode(): Int = board.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case TicTacToe(b, _) => board == b
    case _ => false
  }

  def blocking(x: Boolean): Boolean = false // TODO implement me

  private lazy val center = ((board.value ^ prior.board.value) & 0xC00000) != 0

  private lazy val heuristic: Double = win match {
    case Some(x) if x == player => 8
    case Some(x) if blocking(x) => 7
    case _ => peneWin // TODO we need to be able to distinguish a "fork" position from a single peneWin
    match {
      case Some(x) if x == player => 6
      case _ if center => 3
      case _ => 0
    }

  }

  private def row(board: Board)(i: Int): Row = board.row(i)

  private def rows(board: Board): LazyList[Row] = LazyList.from(0).take(stride).map(i => row(board)(i))

  private def isMatch(f: Matching)(rs: LazyList[Row]): Cell = rs.map(f).foldLeft[Cell](None)((result, cell) => result orElse cell)

  private def isWin(rs: LazyList[Row]): Cell = isMatch(isLine)(rs)

  private def isPendingWin(rs: LazyList[Row]): Cell = isMatch(isLinePending)(rs)

  private def isLine(x: Row): Cell = rowLine(x) match {
    case 1 => Some(true)
    case 2 => Some(false)
    case _ => None
  }

  private def isLinePending(x: Row): Cell = rowLinePending(x) match {
      case 1 => Some(true)
      case 2 => Some(false)
      case _ => None
    }

  import com.phasmidsoftware.util.Flog._

  implicit val optionLoggable: Loggable[Option[Int]] = new Loggables {}.optionLoggable[Int]

  private lazy val r0: Board = board
  private lazy val l0: Board = Board(transposeBoard(board.value))
  private lazy val r1: Board = Board(rotate(r0.value))
  private lazy val l1: Board = Board(rotate(l0.value))
  private lazy val r2: Board = Board(rotate(r1.value))
  private lazy val l2: Board = Board(rotate(l1.value))
  private lazy val r3: Board = Board(rotate(r2.value))
  private lazy val l3: Board = Board(rotate(l2.value))
  private lazy val rowsR0: LazyList[Row] = rows(r0)
  private lazy val rowsL0: LazyList[Row] = rows(l0)
  private lazy val rowsR1: LazyList[Row] = rows(r1)
  private lazy val rowsL1: LazyList[Row] = rows(l1)
  private lazy val rowsR2: LazyList[Row] = rows(r2)
  private lazy val rowsL2: LazyList[Row] = rows(l2)
  private lazy val rowsR3: LazyList[Row] = rows(r3)
  private lazy val rowsL3: LazyList[Row] = rows(l3)
  private lazy val diagR: Row = diagonal(r0.value)
  private lazy val diagL: Row = diagonal(l0.value)
  private lazy val diagonals: LazyList[Row] = diagR #:: diagL #:: LazyList.empty

}

object TicTacToe {
  // XXX the size of the TicTacToe square.
  val stride = 3

  // XXX the starting position (all nine empty cells).
  val start: TicTacToe = apply()

  /**
   * Method to construct a starting position TicTacToe.
   *
   * @return a TicTacToe with all empty cells.
   */
  def apply(): TicTacToe = apply(Board(0))

  /**
   * Method to construct a TicTacToe from a particular bit pattern.
   * NOTE there will not be a valid "prior" (it will just tbe starting pattern).
   *
   * @return a TicTacToe with all empty cells.
   */
  def from(x: Int): TicTacToe = TicTacToe(Board(x))

  /**
   * Method to parse a String of Xs and 0s into a TicTacToe, wrapped in Try.
   *
   * @param s the String to parse.
   * @return a Try of TicTacToe.
   */
  def parse(s: String): Try[TicTacToe] =
    if (s.length == stride * stride) Success(parseString(s))
    else Failure(DecisionTreeException(s"TicTacToe: parse failure: $s"))

  /**
   * Trait which extends the type class State with a concrete underlying type of TicTacToe.
   */
  trait TicTacToeIntState extends State[TicTacToe] {
    /**
     * In this game, all states are valid.
     *
     * @param s a state.
     * @return true.
     */
    def isValid(s: TicTacToe): Boolean = true

    /**
     * How close are we to winning?
     *
     * @param s a state.
     * @return the number of our aligned cells - their aligned cells.
     */
    def heuristic(s: TicTacToe): Double = s.heuristic

    /**
     * Have we reached a result? And, if so, who won?
     *
     * @param s a (current) state.
     * @return a Cell: if None then this state is not a goal state.
     *         If Some(b) then we got a result and the winner is the antagonist who moves first.
     */
    def isGoal(s: TicTacToe): Cell = s.win

    /**
     * Return all of the possible moves from the given state.
     *
     * @param s a state.
     * @return a sequence of Transition[S]
     */
    def moves(s: TicTacToe): Seq[Transition[TicTacToe]] = {
      val zs: Seq[(Int, Int)] = Shuffle(s.open, 3L) // we arbitrarily always want X to win
      val f: TicTacToe => (Int, Int) => TicTacToe = t => if (s.player) t.play0 else t.playX
      for (z <- zs) yield Move[TicTacToe](x => f(x)(z._1, z._2), z.toString())
    }
  }

  implicit object TicTacToeIntState extends TicTacToeIntState

  /**
   * Method to parse a pattern for a starting position.
   * NOTE: do not use for later positions.
   *
   * @param s a String made up of 9 case-independent characters, each of which must be an X, 0, O, ., or space.
   *          CONSIDER allowing newlines.
   * @return a TicTacToe.
   */
  def parseString(s: String): TicTacToe = {
    val cells = s.toCharArray.toSeq map {
      case ' ' | '.' => 0
      case 'X' | 'x' => 1
      case '0' | 'o' | 'O' => 2
      case x => throw DecisionTreeException(s"TicTacToe: illegal character: $x")
    }
    if (cells.length >= 9) TicTacToe(Board(TicTacToeOps.parse(cells.toArray)))
    else throw DecisionTreeException("insufficient elements")
  }
}

///**
// * This class represents just a row of 3 x 2 bits at the low end of the 32-bit word.
// *
// * @param value the bit value of this row.
// */
//case class Row(value: Int) extends AnyVal
