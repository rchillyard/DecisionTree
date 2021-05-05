package com.phasmidsoftware.decisiontree.examples.tictactoe

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe.stride
import com.phasmidsoftware.decisiontree.moves.{Move, State, Transition}
import com.phasmidsoftware.util.DecisionTreeException

import scala.util.{Failure, Success, Try}

case class TicTacToe(board: Seq[Seq[Cell]]) {

  def line: Boolean = rowMatch || colMatch || diagMatch

  def render: Cell => String = {
    case Some(b) => if (b) "X" else "0"
    case None => " "
  }

  override def toString: String = {
    val sb = new StringBuilder
    for (i <- 0 until stride) yield sb.append(s"""${row(i).map(render).mkString("", "", "")}\n""")
    sb.toString
  }

  def row(i: Int): Seq[Cell] = board(i)

  def column(i: Int): Seq[Cell] = for (j <- 0 until stride) yield board(j)(i)

  def diagonal(b: Boolean): Seq[Cell] = if (b)
    for (i <- 0 until stride) yield board(i)(i)
  else
    for (i <- 0 until stride) yield board(stride - i - 1)(i)

  def open: Seq[(Int, Int)] = for (i <- 0 until stride; j <- 0 until stride; if board(i)(j).isEmpty) yield i -> j

  def play(xOrO: Boolean)(row: Int, col: Int): TicTacToe = TicTacToe(playBoard(xOrO)(row, col))

  val playX: (Int, Int) => TicTacToe = play(xOrO = true)
  val play0: (Int, Int) => TicTacToe = play(xOrO = false)

  private def transposeBoard: Seq[Seq[Cell]] = board.transpose

  private def playBoard(xOrO: Boolean)(row: Int, col: Int): Seq[Seq[Cell]] = for (i <- 0 until stride) yield if (i == row) playRow(xOrO)(board(i), col) else board(i)

  private def playRow(xOrO: Boolean)(row: Seq[Cell], col: Int): Seq[Cell] = for (i <- 0 until stride) yield if (i == col) Some(xOrO) else row(i)

  private def same(cs: Seq[Cell]): Boolean = cs.forall(_.contains(true)) || cs.forall(_.contains(false))

  private def rowMatch: Boolean = board.exists(same)

  private def colMatch: Boolean = transposeBoard.exists(same)

  private def diagMatch: Boolean = same(diagonal(true)) || same(diagonal(false))

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
    //    def toPlay(s: TicTacToe): Boolean = (stride * stride - s.open.size) % 2 == 0

    def isValid(s: TicTacToe): Boolean = true

    def heuristic(s: TicTacToe): Double = 0

    def isGoal(s: TicTacToe): Boolean = s.line

    def moves(s: TicTacToe): Seq[Transition[TicTacToe]] = {
      val zs: Seq[(Int, Int)] = s.open
      val toPlay = (stride * stride - zs.size) % 2 == 0
      val f: TicTacToe => (Int, Int) => TicTacToe = t => if (toPlay) t.playX else t.play0
      for (z <- zs) yield Move(x => f(x)(z._1, z._2), z.toString())
    }
  }

  implicit object TicTacToeState extends TicTacToeState

}
