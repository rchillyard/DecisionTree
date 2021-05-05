package com.phasmidsoftware.decisiontree.examples.tictactoe

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe.stride
import com.phasmidsoftware.decisiontree.moves.{Move, State, Transition}
import com.phasmidsoftware.util.{DecisionTreeException, Shuffle}

import scala.util.{Failure, Success, Try}

case class TicTacToe(board: Seq[Seq[Cell]]) {

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
  lazy val line: Boolean = rowMatch || colMatch || diagMatch

  lazy val heuristic: Double = {
    def toInt(b: Boolean): Int = if (b) 1 else 0

    toInt(potentialLine(player)) - toInt(potentialLine(!player))
  }

  /**
   * Method to determine whether there is a potential line of marks of the given player.
   * The line may be horizontal (a row), vertical (a column) or diagonal.
   *
   * @param player true for X, false for 0.
   * @return true if there is a line of player's marks in this TicTacToe.
   */
  def potentialLine(player: Boolean): Boolean = rowMajority(player) || colMajority(player) || diagMajority(player)

  /**
   * Method to create a new TicTacToe from this TicTacToe.
   *
   * @param xOrO true if X is to play, false otherwise.
   * @param row  the row at which the mark should be made.
   * @param col  the column at which the mark should be made.
   * @return a new TicTacToe with the appropriate Cell marked.
   */
  def play(xOrO: Boolean)(row: Int, col: Int): TicTacToe = TicTacToe(playBoard(xOrO)(row, col))

  override def toString: String = {
    val sb = new StringBuilder
    for (i <- 0 until stride) yield sb.append(s"""${row(i).map(render).mkString("", "", "")}\n""")
    sb.toString
  }

  /**
   * Method to yield a row from the board.
   *
   * @param i the row index from 0 thru 2.
   * @return a sequence of Cells.
   */
  def row(i: Int): Seq[Cell] = board(i)

  /**
   * Method to yield a column from the board.
   * This method is slightly less efficient than row.
   * If calling column many times, consider invoking transform first and then calling row.
   *
   * @param i the column index from 0 thru 2.
   * @return a sequence of Cells.
   */
  def column(i: Int): Seq[Cell] = for (j <- 0 until stride) yield board(j)(i)

  /**
   * Val to determine the list of open cells from this TIcTacToe.
   *
   * @return a sequence of (Int, Int) tuples corresponding to the row, column indices.
   */
  lazy val open: Seq[(Int, Int)] = for (i <- 0 until stride; j <- 0 until stride; if board(i)(j).isEmpty) yield i -> j

  /**
   * Method to determine if there's a diagonal of one mark.
   *
   * @param b the orientation of the diagonal: true: 0,0 thru 2,2 otherwise 2,0 thru 0,2.
   * @return a sequence of Cells.
   */
  def diagonal(b: Boolean): Seq[Cell] = if (b)
    for (i <- 0 until stride) yield board(i)(i)
  else
    for (i <- 0 until stride) yield board(stride - i - 1)(i)

  val playX: (Int, Int) => TicTacToe = play(xOrO = true)
  val play0: (Int, Int) => TicTacToe = play(xOrO = false)

  /**
   * Function to render a Cell.
   */
  private val render: Cell => String = {
    case Some(b) => if (b) "X" else "0"
    case None => " "
  }

  private def transposeBoard: Seq[Seq[Cell]] = board.transpose

  private def playBoard(xOrO: Boolean)(row: Int, col: Int): Seq[Seq[Cell]] = for (i <- 0 until stride) yield if (i == row) playRow(xOrO)(board(i), col) else board(i)

  private def playRow(xOrO: Boolean)(row: Seq[Cell], col: Int): Seq[Cell] = for (i <- 0 until stride) yield if (i == col) Some(xOrO) else row(i)

  private def same(cs: Seq[Cell]): Boolean = matching(cs)(player = true) || matching(cs)(player = false)

  // CONSIDER Do we really need to specify the player?
  private def matching(cs: Seq[Cell])(player: Boolean) = cs.forall(_.contains(player))

  private def isMajority(cs: Seq[Cell])(player: Boolean): Boolean = cs.count(_.contains(player)) == 2 && cs.count(_.isEmpty) == 1

  private def majority(cs: Seq[Cell])(player: Boolean): Boolean = isMajority(cs)(player)

  private lazy val rowMatch: Boolean = board.exists(same)

  private lazy val colMatch: Boolean = transposeBoard.exists(same)

  private lazy val diagMatch: Boolean = same(diagonal(true)) || same(diagonal(false))

  private def rowMajority(player: Boolean): Boolean = board.exists(majority(_)(player))

  private def colMajority(player: Boolean): Boolean = transposeBoard.exists(majority(_)(player))

  private def diagMajority(player: Boolean): Boolean = majority(diagonal(true))(player) || majority(diagonal(false))(player)
}

object TicTacToe {
  val stride = 3

  val empty: Seq[Seq[Cell]] = List.fill(stride)(List.fill(stride)(None))

  val start: TicTacToe = apply()

  def apply(): TicTacToe = apply(empty)

  private def parseString(s: String): TicTacToe = {
    val chars = s.toCharArray.toSeq
    val cells: Seq[Cell] = chars map {
      case ' ' => None
      case 'X' => Some(true)
      case '0' => Some(false)
      case x => throw DecisionTreeException(s"TicTacToe: illegal character: $x")
    }
    val board: Seq[Seq[Cell]] = cells.grouped(stride).toSeq
    TicTacToe(board)
  }

  def parse(s: String): Try[TicTacToe] =
    if (s.length == stride * stride) Success(parseString(s))
    else Failure(DecisionTreeException(s"TicTacToe: parse failure: $s"))

  trait TicTacToeState extends State[TicTacToe] {
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
     * @return an Option of Boolean: if None then this state is not a goal state.
     *         If Some(b) then we got a result and the winner is the antagonist who moves first.
     */
    def isGoal(s: TicTacToe): Option[Boolean] =
      if (s.line)
        Some(s.player)
      else
        None

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

  implicit object TicTacToeState extends TicTacToeState

}
