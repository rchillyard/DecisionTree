package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe

object EvaluatorApp extends App {
    val eval = new Evaluator[TicTacToe]
    val start: TicTacToe = TicTacToe()
    val so: Option[TicTacToe] = eval.evaluate(start)
    println(so)
    println(so map (implicitly[State[TicTacToe]].isGoal(_)))
}