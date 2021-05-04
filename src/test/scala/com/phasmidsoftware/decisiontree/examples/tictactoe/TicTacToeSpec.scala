package com.phasmidsoftware.decisiontree.examples.tictactoe

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe.stride
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success}

class TicTacToeSpec extends AnyFlatSpec with should.Matchers {

  behavior of "TicTacToeSpec"

  it should "row" in {
    val ttt = TicTacToe()
    ttt.row(0).size shouldBe stride
    ttt.row(0) shouldBe Seq(None, None, None)
  }

  it should "column" in {
    val ttt = TicTacToe()
    ttt.column(0).size shouldBe stride
    ttt.column(0) shouldBe Seq(None, None, None)
  }

  it should "open" in {
    val ttt = TicTacToe()
    ttt.open.size shouldBe stride * stride
  }

  it should "play true, 0, 0" in {
    val t0 = TicTacToe()
    val t1 = t0.play(xOrO = true)(0, 0)
    t1.open.size shouldBe stride * stride - 1
    t1.toString shouldBe "X  \n   \n   \n"
  }

  it should "play false, 1, 0" in {
    val t0 = TicTacToe()
    val t1 = t0.play(xOrO = false)(1, 0)
    t1.open.size shouldBe stride * stride - 1
      t1.toString shouldBe "   \n0  \n   \n"
  }

    it should "playX 0, 0 and play0 1, 0" in {
        val t0 = TicTacToe()
        val t1 = t0.playX(0, 0)
        val t2 = t1.play0(1, 0)
        t2.open.size shouldBe stride * stride - 2
        t2.toString shouldBe "X  \n0  \n   \n"
    }

    it should "parse" in {
        TicTacToe.parse("         ") should matchPattern { case Success(TicTacToe.start) => }
        val ty1 = TicTacToe.parse("X        ")
        ty1 should matchPattern { case Success(_) => }
        ty1.get shouldBe TicTacToe().playX(0, 0)
        val ty2 = TicTacToe.parse("X  0     ")
        ty2 should matchPattern { case Success(_) => }
        ty2.get shouldBe TicTacToe().playX(0, 0).play0(1, 0)
    }

    it should "not parse" in {
        TicTacToe.parse("") should matchPattern { case Failure(_) => }
        TicTacToe.parse("        ") should matchPattern { case Failure(_) => }
        TicTacToe.parse("          ") should matchPattern { case Failure(_) => }
    }

    it should "line row" in {
        TicTacToe.parse("XXX      ").get.line shouldBe true
        TicTacToe.parse("   XXX   ").get.line shouldBe true
        TicTacToe.parse("      XXX").get.line shouldBe true
        TicTacToe.parse("0     XXX").get.line shouldBe true
        TicTacToe.parse("000   X  ").get.line shouldBe true
    }

    it should "line col" in {
        TicTacToe.parse("X  X  X  ").get.line shouldBe true
        TicTacToe.parse(" X 0X  X ").get.line shouldBe true
        TicTacToe.parse("0 X  X  X").get.line shouldBe true
    }

    it should "line diag" in {
        TicTacToe.parse("XXX     0").get.line shouldBe true
        TicTacToe.parse("   XXX0  ").get.line shouldBe true
        TicTacToe.parse(" 0    XXX").get.line shouldBe true
    }
}
