package com.phasmidsoftware.decisiontree.examples.tictactoe

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe.{Prototype, size}
import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToeOps._
import com.phasmidsoftware.decisiontree.moves.{Move, State, Transition}
import com.phasmidsoftware.flog.Loggable
import com.phasmidsoftware.util.Aggregators.{hasOne, hasTwo}
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
   * Method to determine whether there are two potential wins.
   * The lines may be horizontal (a row), vertical (a column) or diagonal and missing one element.
   *
   * @return true if there is a line of two similar marks with a space between in this TicTacToe.
   */
  lazy val fork: Cell = hasTwo(for (r <- List(rowsR0, rowsL0, diagonals)) yield isPendingWin(r))(_.isDefined) match {
    case Some(x -> _) => x
    case None => None
  }

  /**
   * Method to determine whether there is a potential win.
   * The line may be horizontal (a row), vertical (a column) or diagonal and missing one element.
   *
   * @return true if there is a line of two similar marks with a space between in this TicTacToe.
   */
  lazy val peneWin: Cell = hasOne(for (r <- List(rowsR0, rowsL0, diagonals)) yield isPendingWin(r))(_.isDefined).flatten

  /**
   * Method to create a new TicTacToe from this TicTacToe.
   *
   * @param xOrO true if X is to play, false otherwise.
   * @param row  the row at which the mark should be made.
   * @param col  the column at which the mark should be made.
   * @return a new Board with the appropriate Cell marked.
   */
  def play(xOrO: Boolean)(row: Int, col: Int): Prototype = board.play(xOrO, row, col) -> this

  /**
   * Method to create a string of Xs and 0s corresponding to this TicTacToe position.
   * Also includes the heuristic for the position.
   *
   * CONSIDER adding a parameter to allow dropping the newlines and/or the heuristic.
   *
   * @return a String which is a rendition of the current state.
   */
  def render(): String = s"\n${board.render} ($heuristic)"

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
          fork match {
            case Some(x) if x == player => 5
            case _ => peneWin match {
              case Some(x)
                if x == player => 4
              case _
                // NOTE: In theory, it doesn't matter whether the first X goes in the center or a top-left corner.
                // Nevertheless, we force the top-left corner.
                if firstAndTopLeftCorner => 4
              case _
                if center => 3
              case _
                if oppositeCorner(true) => 2
              case _
                if corner && oppHasCenter && weHaveOppositeCorner => 2
              case _
                if corner => 1
              case _ =>
                0
            }
          }
      }
  }

  private lazy val center = (currentMove & 0xC00000) != 0

  lazy val currentMove: Row = maybePrior map (p => board.value ^ p.board.value) getOrElse board.value

  lazy val maybeOpponentMove: Option[Row] = maybePrior map (_.currentMove)

  lazy val maybePreviousMove: Option[Row] = maybePrior flatMap (_.maybeOpponentMove)

  /**
   * @param opponent if true then the opposite corner must be the opponent.
   * @return true if this TicTacToe is a corner position and if the opponent (if true, else self) occupies the opposite corner.
   */
  def oppositeCorner(opponent: Boolean): Boolean = {
    val maskTopLeft = 0xC0000000
    val patternTopLeft = if (opponent) maskTopLeft else 0
    val f = maskMatch(maskTopLeft, patternTopLeft)(_, _)
    corner && (f(r0, r2) || f(r1, r3))
  }

  /**
   * @param b1 a Board.
   * @param b2 another Board.
   * @return true if the XOR of the two boards masked by mask is equal to pattern.
   */
  def maskMatch(mask: Row, pattern: Row)(b1: Board, b2: Board): Boolean = ((b1.value ^ b2.value) & mask) == pattern

  lazy val corner: Boolean = (currentMove & 0xCC0CC000) != 0

  def oppHasCenter: Boolean = maybePrior exists (_.center)

  def weHaveOppositeCorner: Boolean = oppositeCorner(false) //maybePrior flatMap (_.maybePrior) exists (_.oppositeCorner)

  private lazy val firstAndTopLeftCorner = board.value == 0x40000000

  private def row(board: Board)(i: Int): Row = board.row(i)

  private def rowsWithMask(board: Board, mask: Row) = LazyList.from(0).take(size).map(i => row(board)(i) -> TicTacToeOps.row(mask, i))

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

  private lazy val differenceTransposed: Row = transposeBoard(currentMove)
  private lazy val r0: Board = board
  private lazy val l0: Board = board.transpose
  private lazy val r1: Board = r0.rotate
  private lazy val r2: Board = r1.rotate
  private lazy val r3: Board = r2.rotate
  private lazy val rowsR0: LazyList[RowWithMask] = rowsWithMask(r0, currentMove)
  private lazy val rowsL0: LazyList[RowWithMask] = rowsWithMask(l0, differenceTransposed)
  private lazy val diagR: RowWithMask = diagonal(r0.value) -> diagonal(currentMove)
  private lazy val diagL: RowWithMask = diagonal(l0.value) -> diagonal(differenceTransposed)
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
     * Method to construct an S from a proto-state:
     *
     * @param proto a (Board, TicTacToe) tuple.
     * @return a TicTacToe.
     */
    def construct(proto: (Board, TicTacToe)): TicTacToe = TicTacToe(proto._1, Some(proto._2))

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
    //    {
    //      import flog._
    //      s"Heuristic for $s" !! s.heuristic
    //    }

    /**
     * Method to determine if s is a winning state.
     * NOTE: it makes no sense to invoke isWin unless the result of isGoal is Some(true).
     *
     * @param s an S
     * @return true if s is a win, else false.
     */
    def isWin(s: TicTacToe): Boolean = s.win match {
      case Some(b) => b == s.player
      case _ => false
    }

    /**
     * Have we reached a result? And, if so, who won?
     *
     * @param s a (current) state.
     * @return a Cell: if None then this state is not a goal state.
     *         If Some(b) then:
     *         if b is true, we got a definite result and the winner is determined by examining s in more detail.
     *         If b is false, we got a partial result and we should continue to seek a more definite result.
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

  private def previous(value: Row, mask: Row): TicTacToe = TicTacToe(Board(value ^ mask))

  /**
   * Apply method mostly for testing.
   *
   * @param board the current Board.
   * @param mask  a mask which defines the bits to be eliminated from board to yield the previous Board.
   * @return a TicTacToe.
   */
  def apply(board: Board, mask: Int): TicTacToe = TicTacToe(board, Some(previous(board.value, mask)))

  /**
   * Method to construct a TicTacToe with prior from a particular bit pattern and a mask.
   *
   * @param x         the bit pattern.
   * @param maybeMask an optional bit pattern to yield the prior state.
   * @return a TicTacToe with all empty cells.
   */
  def from(x: Int, maybeMask: Option[Int]): TicTacToe = TicTacToe(Board(x), maybeMask.map(previous(x, _)))

  /**
   * Method to parse a pattern for a starting position.
   * NOTE: do not use for later positions.
   *
   * @param s a String made up of 9 case-independent characters, each of which must be an X, 0, O, ., or space.
   *          CONSIDER allowing newlines.
   * @return a TicTacToe.
   */
  def parseString(s: String, maybeMask: Option[Row]): TicTacToe = {
    val cells = s.toCharArray.toSeq map {
      case ' ' | '.' => 0
      case 'X' | 'x' => 1
      case '0' | 'o' | 'O' => 2
      case x => throw DecisionTreeException(s"TicTacToe: illegal character: $x")
    }
    if (cells.length >= 9) TicTacToe.from(TicTacToeOps.parse(cells.toArray), maybeMask)
    else throw DecisionTreeException("insufficient elements")
  }

  /**
   * Method to parse a String of Xs and 0s into a TicTacToe, wrapped in Try.
   *
   * CONSIDER making this private.
   *
   * @param s the String to parse (which may include \n characters for better visualization).
   * @return a Try of TicTacToe.
   */
  def parse(s: String, maybeMask: Option[Int] = None): Try[TicTacToe] = {
    val x = s.replaceAll("""\n""", "")
    if (x.length == TicTacToe.size * TicTacToe.size) Success(parseString(x, maybeMask))
    else Failure(DecisionTreeException(s"TicTacToe: parse failure: $s"))
  }

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

  override def toString: String = render // value.toHexString

  def render: String = TicTacToeOps.render(value)

  def play(xOrO: Boolean, row: Row, col: Row): Board = Board(playBoard(value, xOrO, row, col))

  def transpose: Board = Board(transposeBoard(value))

  def rotate: Board = Board(rotateBoard(value))

  def exchange: Board = Board(exchangeBoard(value))
}
