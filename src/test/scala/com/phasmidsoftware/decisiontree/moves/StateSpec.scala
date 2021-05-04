package com.phasmidsoftware.decisiontree.moves

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class StateSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Move"

  it should "apply" in {
    val move: Move[Chess] = Move(identity, "no-move")
    val start = Chess("")
    move.apply(start) shouldBe start
  }

  it should "description" in {
    val move: Move[Chess] = Move(identity, "no-move")
    move.description shouldBe "no-move"
  }

  behavior of "ChessState"
  it should "work" in {
    val z = implicitly[State[Chess]]
    val chess = Chess("")
    val checkmate = Chess("checkmate")
    z.isValid(chess) shouldBe true
    z.moves(chess) shouldBe Nil
    z.isGoal(chess) shouldBe false
    z.heuristic(chess) shouldBe 0
  }
}

