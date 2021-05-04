package com.phasmidsoftware.decisiontree.examples.tictactoe

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe.stride

case class TicTacToe(board: Seq[Seq[Cell]]) {

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

  def open: Seq[(Int, Int)] = for (i <- 0 until stride; j <- 0 until stride; if board(i)(j).isEmpty) yield i -> j

  def play(xOrO: Boolean)(row: Int, col: Int): TicTacToe = TicTacToe(playBoard(xOrO)(row, col))

  val playX: (Int, Int) => TicTacToe = play(xOrO = true)
  val play0: (Int, Int) => TicTacToe = play(xOrO = false)

  private def playBoard(xOrO: Boolean)(row: Int, col: Int): Seq[Seq[Cell]] = for (i <- 0 until stride) yield if (i == row) playRow(xOrO)(board(i), col) else board(i)

  private def playRow(xOrO: Boolean)(row: Seq[Cell], col: Int): Seq[Cell] = for (i <- 0 until stride) yield if (i == col) Some(xOrO) else row(i)
}

object TicTacToe {
  val stride = 3

  val empty: Seq[Seq[Cell]] = List.fill(stride)(List.fill(stride)(None))

  def apply(): TicTacToe = apply(empty)
}
