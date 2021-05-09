package com.phasmidsoftware.decisiontree.examples.tictactoe

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe.stride
import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToeInt.start
import com.phasmidsoftware.decisiontree.moves.{Move, State, Transition}
import com.phasmidsoftware.util.{DecisionTreeException, Loggable, Loggables, Shuffle}

import scala.util.{Failure, Success, Try}

/**
 * This class represents 9 x 2 bits, at the high end of the 32-bit word.
 *
 * @param value the bit value of this row.
 */
case class Board(value: Int) extends AnyVal {
  def row(i: Int): Row = TicTacToeIntOperations.row(value, i)

  def toHexString: String = value.toHexString

  def transpose: Board = Board(TicTacToeIntOperations.transposeBoard(value))
}

/**
 * This class represents just a row of 3 x 2 bits at the low end of the 32-bit word.
 *
 * @param value the bit value of this row.
 */
case class Row(value: Int) extends AnyVal

case class TicTacToeInt(board: Board, prior: TicTacToeInt = start) {

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

  // TODO make private once private method tester is working
  def row(i: Int): Row = board.row(i)

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
  def play(xOrO: Boolean)(row: Int, col: Int): TicTacToeInt = TicTacToeInt(Board(TicTacToeIntOperations.play(board.value, xOrO, row, col)), this)

  def render: String = s"${TicTacToeIntOperations.render(board.value)} ($heuristic)"

  override def toString: String = s"${board.toHexString}"

  /**
   * Val to determine the list of open cells from this TIcTacToe.
   *
   * @return a sequence of (Int, Int) tuples corresponding to the row, column indices.
   */
  lazy val open: Seq[(Int, Int)] = {
    val zs: Array[Int] = TicTacToeIntOperations.open(board.value)
    val q = for (z <- zs) yield z / stride -> z % stride
    q.toList // CONSIDER returning q as is.
  }

  val playX: (Int, Int) => TicTacToeInt = play(xOrO = true)
  val play0: (Int, Int) => TicTacToeInt = play(xOrO = false)

  override def hashCode(): Int = board.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case TicTacToeInt(b, _) => board == b
    case _ => false
  }

  def blocking(x: Boolean): Boolean = false // TODO implement me

  private lazy val center = ((board.value ^ prior.board.value) & 0xC00000) != 0

  private lazy val heuristic: Double = line match {
    case Some(x) if x == player => 8
    case Some(x) if blocking(x) => 7
    case _ => pendingLine // TODO we need to be able to distinguish a "fork" position from a single pendingLine
    match {
      case Some(x) if x == player => 6
      case _ if center => 3
      case _ => 0
    }

  }

  def transpose: TicTacToeInt = TicTacToeInt(board.transpose)

  private def rowDiagMatch(f: Row => Cell): Cell = LazyList.from(0).take(3).map(row).map(f).foldLeft[Cell](None)((r, c) => r orElse c) orElse diagMatch(f)

  private def diagMatch(f: Row => Cell): Cell = f(TicTacToeIntOperations.diagonal(board.value))

  private def isLine(x: Row): Cell = TicTacToeIntOperations.rowLine(x) match {
    case 1 => Some(true)
    case 2 => Some(false)
    case _ => None
  }

  import com.phasmidsoftware.util.Flog._

  implicit val optionLoggable: Loggable[Option[Int]] = new Loggables {}.optionLoggable[Int]

  def rowDiagPendingMatch(f: Row => Option[Int]): Option[Int] = s"rowDiagPendingMatch: $board" !!
    LazyList.from(0).take(3).map(row).map(f).foldLeft[Option[Int]](None)((r, c) => r & c) & diagPendingMatch(f)

  def isPendingLine(x: Row): Option[Int] =
    TicTacToeIntOperations.rowLinePending(TicTacToeIntOperations.row(board.value, x)) match {
      case 1 => Some(0)
      case 2 => Some(1)
      case _ => None
    }

  private def diagPendingMatch(f: Row => Option[Int]): Option[Int] = f(TicTacToeIntOperations.diagonal(board.value))
}

object TicTacToeInt {
  val stride = 3

  val start: TicTacToeInt = apply()

  def apply(): TicTacToeInt = apply(Board(0))

  //  def apply(b: Board): TicTacToeInt = TicTacToeInt(b)

  def from(x: Int): TicTacToeInt = TicTacToeInt(Board(x))

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
     * @return a Cell: if None then this state is not a goal state.
     *         If Some(b) then we got a result and the winner is the antagonist who moves first.
     */
    def isGoal(s: TicTacToeInt): Cell = s.line

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

  /**
   * Method to parse a pattern for a starting position.
   * NOTE: do not use for later positions.
   *
   * @param s a String made up of 9 case-independent characters, each of which must be an X, 0, O, ., or space.
   *          CONSIDER allowing newlines.
   * @return a TicTacToeInt.
   */
  def parseString(s: String): TicTacToeInt = {
    val cells = s.toCharArray.toSeq map {
      case ' ' | '.' => 0
      case 'X' | 'x' => 1
      case '0' | 'o' | 'O' => 2
      case x => throw DecisionTreeException(s"TicTacToeInt: illegal character: $x")
    }
    if (cells.length >= 9) TicTacToeInt(Board(TicTacToeIntOperations.parse(cells.toArray)))
    else throw DecisionTreeException("insufficient elements")
  }
}

