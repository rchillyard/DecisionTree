package com.phasmidsoftware.decisiontree.examples.tictactoe

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe.stride
import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToeIntOperations._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success}

class TicTacToeIntSpec extends AnyFlatSpec with should.Matchers {

    behavior of "TicTacToeIntOperations"
    it should "render" in {
        render(0x40000000) shouldBe "X..\n...\n...\n"
    }
    it should "play" in {
        val board1 = play(0, true, 0, 0)
        board1 shouldBe 0x40000000
        val board2 = play(board1, false, 2, 0)
        render(board2) shouldBe "X..\n...\n0..\n"
        board2 shouldBe 0x40080000
    }
    it should "transpose to Int" in {
        transpose(0xC0000000) shouldBe 0xC0000000
        transpose(0x30000000) shouldBe 0x03000000
        transpose(0x0C000000) shouldBe 0x000C0000
        transpose(0x03000000) shouldBe 0x30000000
        transpose(0x00C00000) shouldBe 0xc00000
        transpose(0x00300000) shouldBe 0x30000
        transpose(0x000C0000) shouldBe 0xc000000
        transpose(0x00030000) shouldBe 0x300000
        transpose(0x0000C000) shouldBe 0xc000
    }
    it should "transpose to hex string" in {
        transpose(0xC0000000).toHexString shouldBe "c0000000"
        transpose(0x30000000).toHexString shouldBe "3000000"
        transpose(0x0C000000).toHexString shouldBe "c0000"
        transpose(0x03000000).toHexString shouldBe "30000000"
        transpose(0x00C00000).toHexString shouldBe "c00000"
        transpose(0x00300000).toHexString shouldBe "30000"
        transpose(0x000C0000).toHexString shouldBe "c000000"
        transpose(0x00030000).toHexString shouldBe "300000"
        transpose(0x0000C000).toHexString shouldBe "c000"
    }
    it should "transpose to Int 2" in {
        transpose(0xF0000000) shouldBe 0xC3000000
        transpose(0x03C00000) shouldBe 0x30c00000
    }
    it should "get rows" in {
        row(0xC0000000, 0) shouldBe 0x30
        row(0x03000000, 1) shouldBe 0x30
        row(0x00300000, 1) shouldBe 0x03
        row(0x00030000, 2) shouldBe 0x0C
    }
    it should "get diag" in {
        diagonal(0xC0000000) shouldBe 0x30
        diagonal(0xC00000) shouldBe 0xc
        diagonal(0xC000) shouldBe 3
    }

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
