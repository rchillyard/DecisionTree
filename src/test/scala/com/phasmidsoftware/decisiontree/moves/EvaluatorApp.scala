package com.phasmidsoftware.decisiontree.moves

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToeSlow

object EvaluatorApp extends App {
    val eval = new Evaluator[TicTacToeSlow]
    val start: TicTacToeSlow = TicTacToeSlow()
    val so: Option[TicTacToeSlow] = eval.evaluate(start)
    println(so)
    println(so map (implicitly[State[TicTacToeSlow]].isGoal(_)))
}