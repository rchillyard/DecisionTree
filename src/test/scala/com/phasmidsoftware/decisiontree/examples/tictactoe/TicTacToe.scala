package com.phasmidsoftware.decisiontree.examples.tictactoe

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe.{Prototype, size}
import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToeOps._
import com.phasmidsoftware.decisiontree.moves.{Move, State, Transition}
import com.phasmidsoftware.flog.Loggable
import com.phasmidsoftware.util.{DecisionTreeException, Shuffle}

import scala.util.{Failure, Success, Try}

/**
 * Case class to represent a TicTacToe state (layout).
 *
 * @param board      the current state.
 * @param maybePrior the prior state.
 */
case class TicTacToe(board: Board, maybePrior: Option[TicTacToe] = None) {

  /**
   * Defines a Matching type which takes a Row and returns a Cell.
   */
  type Matching = RowWithMask => Cell

  /**
   * Method to determine which player was responsible for creating this state.
   * The empty state will yield false (0 player).
   * A goal state will yield the winner.
   *
   * @return true for the first player (X), false for the second player (0)
   */
  lazy val player: Boolean = (size * size - open.size) % 2 == 1

  /**
   * Method to determine whether there is a line of marks for one player.
   * The line may be horizontal (a row), vertical (a column) or diagonal.
   *
   * @return Some(b) if there is a line of similar marks in this TicTacToe otherwise None.
   *         The Boolean b is true for player X, and false for player 0.
   */
  lazy val win: Cell = isWin(rowsR0) orElse isWin(rowsL0) orElse isWin(diagonals)

  /**
   * Method to determine whether there is a block by one player against his opponent..
   * The line may be horizontal (a row), vertical (a column) or diagonal.
   *
   * @return Some(b) if there is a line of 0X0 or similar otherwise None.
   *         The Boolean b is true for player X, and false for player 0.
   */
  lazy val block: Cell = isBlock(rowsR0) orElse isBlock(rowsL0) orElse isBlock(diagonals)

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
   * @return a new Board with the appropriate Cell marked.
   */
  def play(xOrO: Boolean)(row: Int, col: Int): Prototype = Board(TicTacToeOps.play(board.value, xOrO, row, col)) -> this

  /**
   * Method to create a string of Xs and 0s corresponding to this TicTacToe position.
   * Also includes the heuristic for the position.
   *
   * CONSIDER adding a parameter to allow dropping the newlines and/or the heuristic.
   *
   * @return a String which is a rendition of the current state.
   */
  def render(): String = s"\n${TicTacToeOps.render(board.value)} ($heuristic)"

  /**
   * The history of a TicTacToe position, as a String.
   */
  lazy val history: List[String] = maybePrior match {
    case None => List("")
    case Some(x) => x.history :+ render()
  }

  /**
   * toString method for debugging: give the current board as a Hex String.
   *
   * @return a String of hexadecimal characters of length 8.
   */
  override def toString: String = s"$board"

  /**
   * The list of open cells for this TicTacToe.
   *
   * @return a sequence of (Int, Int) tuples corresponding to the row, column indices.
   */
  lazy val open: Seq[(Int, Int)] = {
    val zs: Array[Int] = TicTacToeOps.open(board.value)
    val q = for (z <- zs) yield z / size -> z % size
    q.toList // CONSIDER returning q as is.
  }

  /**
   * Function to make a play for the X player at a cell.
   */
  def playX: (Int, Int) => Prototype = play(xOrO = true)(_, _)

  /**
   * Function to make a play for the 0 player at a cell.
   */
  def play0: (Int, Int) => Prototype = play(xOrO = false)(_, _)

  /**
   * HashCode method based on the board only (not prior).
   *
   * @return a hashCode.
   */
  override def hashCode(): Int = board.hashCode()

  /**
   * Equals method based on the board only.
   *
   * @return Boolean.
   */
  override def equals(obj: Any): Boolean = obj match {
    case TicTacToe(b, _) => board == b
    case _ => false
  }

  /**
   * Method to determine if this layout is a draw, i.e. no side can ever win.
   *
   * TODO figure out a better way to determine a draw.
   *
   * @return Some(false) if it's a draw, else None.
   */
  def draw: Cell = if (open.isEmpty) Some(false) else None

  private lazy val heuristic: Double = win match {
    case Some(x)
      if x == player => 7
    case _ =>
      block match {
        case Some(y)
          if y == player => 6
        case _ =>
          peneWin // TODO we need to be able to distinguish a "fork" position from a single peneWin
          match {
            case Some(x)
              if x == player => 5
            case _
              if center => 3
            case _
              if oppositeCorner => 2
            case _
              if corner => 1
            case _ =>
              0
          }
      }
  }

  private lazy val center = (difference & 0xC00000) != 0

  private lazy val difference = maybePrior map (p => board.value ^ p.board.value) getOrElse board.value

  private lazy val oppositeCorner = corner && (opposite(r0, r2) || opposite(r1, r3))

  private def opposite(r0: Board, r1: Board): Boolean = ((r0.value ^ r1.value) & 0xC0000000) == 0xC0000000

  private lazy val corner = (difference & 0xCC0CC000) != 0

  private def row(board: Board)(i: Int): Row = board.row(i)

  private def rowsWithMask(board: Board): LazyList[RowWithMask] = LazyList.from(0).take(size).map(i => row(board)(i) -> TicTacToeOps.row(difference, i))

  private def isMatch(f: Matching)(rs: LazyList[RowWithMask]): Cell = rs.map(f).foldLeft[Cell](None)((result, cell) => result orElse cell)

  private def isWin(rs: LazyList[RowWithMask]): Cell = isMatch(isLine)(rs)

  private def isPendingWin(rs: LazyList[RowWithMask]): Cell = isMatch(isLinePending)(rs)

  private def isBlock(rs: LazyList[RowWithMask]): Cell = isMatch(isBlocking)(rs)

  private def isLine(x: RowWithMask): Cell = rowLine(x._1) match {
    case 1 => Some(true)
    case 2 => Some(false)
    case _ => None
  }

  private def isLinePending(x: RowWithMask): Cell = rowLinePending(x._1) match {
    case 1 => Some(true)
    case 2 => Some(false)
    case _ => None
  }

  private def isBlocking(x: RowWithMask): Cell = rowLineBlocking(x._1, x._2) match {
    case 1 => Some(true)
    case 2 => Some(false)
    case _ => None
  }

  private lazy val r0: Board = board
  private lazy val l0: Board = Board(transposeBoard(board.value))
  private lazy val r1: Board = Board(rotate(r0.value))
  private lazy val r2: Board = Board(rotate(r1.value))
  private lazy val r3: Board = Board(rotate(r2.value))
  private lazy val rowsR0: LazyList[RowWithMask] = rowsWithMask(r0)
  private lazy val rowsL0: LazyList[RowWithMask] = rowsWithMask(l0)
  private lazy val diagR: RowWithMask = diagonal(r0.value) -> diagonal(difference)
  private lazy val diagL: RowWithMask = diagonal(l0.value) -> diagonal(transposeBoard(difference))
  private lazy val diagonals: LazyList[RowWithMask] = diagR #:: diagL #:: LazyList.empty
}

object TicTacToe {

  type Prototype = (Board, TicTacToe)

  val size: Int = 3

  /**
   * Trait which extends the type class State with a concrete underlying type of TicTacToe.
   */
  trait TicTacToeState$ extends State[Board, TicTacToe] {

    /**
     * Method to construct an S from the following parameters:
     *
     * @param proto a (Board, TicTacToe) tuple.
     * @param q     a PriorityQueue.
     * @return an S.
     */
    def construct(proto: (Board, TicTacToe)): TicTacToe = TicTacToe(proto._1, Some(proto._2))

    /**
     * Method to yield the previous state.
     *
     * @return an optional State[S].
     */
    def previous(s: TicTacToe): Option[TicTacToe] = s.maybePrior

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
    def isGoal(s: TicTacToe): Cell = s.win orElse s.draw

    /**
     * Return all of the possible transitions from the given state.
     *
     * CONSIDER refactoring so that we do not discard the state when constructing a Move.
     *
     * @param s a state.
     * @return a sequence of Transition[S]
     */
    def moves(s: TicTacToe): Seq[Transition[Board, TicTacToe]] = {
      val zs: Seq[(Int, Int)] = Shuffle(s.open, 3L) // we arbitrarily always want X to win
      val f: TicTacToe => (Int, Int) => Prototype = t => if (s.player) t.play0 else t.playX
      for (z <- zs) yield Move[Board, TicTacToe](x => f(x)(z._1, z._2)._1, z.toString())
    }
  }

  implicit object TicTacToeState$ extends TicTacToeState$

  /**
   * Method to construct a starting position TicTacToe.
   *
   * @param board the Board which defines a TicTacToe.
   * @return a TicTacToe with the given board, no predecessor and an empty Priority Queue.
   */
  def apply(board: Board): TicTacToe = apply(board, None)

  /**
   * Method to construct a starting position TicTacToe.
   *
   * @param proto (Board, TicTacToe).
   * @return a TicTacToe with the given board, no predecessor and an empty Priority Queue.
   */
  def apply(proto: (Board, TicTacToe)): TicTacToe = apply(proto._1, Some(proto._2))

  /**
   * Method to construct a starting position TicTacToe.
   *
   * @return a TicTacToe with all empty cells, no predecessor and an empty Priority Queue.
   */
  def apply(): TicTacToe = apply(Board(0))

  /**
   * Method to construct a TicTacToe from a particular bit pattern.
   * NOTE there will not be a valid "prior" (it will just tbe starting pattern).
   *
   * @return a TicTacToe with all empty cells.
   */
  def from(x: Int): TicTacToe = TicTacToe(Board(x), None)

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
    if (cells.length >= 9) TicTacToe.from(TicTacToeOps.parse(cells.toArray))
    else throw DecisionTreeException("insufficient elements")
  }

  /**
   * Method to parse a String of Xs and 0s into a TicTacToe, wrapped in Try.
   *
   * CONSIDER making this private.
   *
   * @param s the String to parse.
   * @return a Try of TicTacToe.
   */
  def parse(s: String): Try[TicTacToe] =
    if (s.length == TicTacToe.size * TicTacToe.size) Success(parseString(s))
    else Failure(DecisionTreeException(s"TicTacToe: parse failure: $s"))

  // XXX the starting position (all nine empty cells).
  val start: TicTacToe = apply()

  implicit val loggableTicTacToe: Loggable[TicTacToe] = (t: TicTacToe) => t.render()
}


/**
 * This class represents 9 x 2 bits, at the high end of the 32-bit word.
 *
 * @param value the bit value of this row.
 */
case class Board(value: Int) extends AnyVal {
  def row(i: Int): Row = TicTacToeOps.row(value, i)

  override def toString: String = value.toHexString

  def render: String = TicTacToeOps.render(value)

  def transpose: Board = Board(transposeBoard(value))
}

///**
// * This class represents just a row of 3 x 2 bits at the low end of the 32-bit word.
// *
// * @param value the bit value of this row.
// */
//case class Row(value: Int) extends AnyVal
