package com.phasmidsoftware.decisiontree.examples.tictactoe

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToeInt.stride
import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToeIntOperations._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success, Try}

class TicTacToeIntSpec extends AnyFlatSpec with should.Matchers {

    behavior of "TicTacToeIntOperations"
    it should "open" in {
        TicTacToeIntOperations.open(0) shouldBe Array(0, 1, 2, 3, 4, 5, 6, 7, 8)
        TicTacToeIntOperations.open(0x40000000) shouldBe Array(1, 2, 3, 4, 5, 6, 7, 8)
        TicTacToeIntOperations.open(0x0C000000) shouldBe Array(0, 1, 2, 3, 4, 5, 6, 7, 8)
        TicTacToeIntOperations.open(0x40400000) shouldBe Array(1, 2, 3, 5, 6, 7, 8)
    }
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
        transposeBoard(0xC0000000) shouldBe 0xC0000000
        transposeBoard(0x30000000) shouldBe 0x03000000
        transposeBoard(0x0C000000) shouldBe 0x000C0000
        transposeBoard(0x03000000) shouldBe 0x30000000
        transposeBoard(0x00C00000) shouldBe 0xc00000
        transposeBoard(0x00300000) shouldBe 0x30000
        transposeBoard(0x000C0000) shouldBe 0xc000000
        transposeBoard(0x00030000) shouldBe 0x300000
        transposeBoard(0x0000C000) shouldBe 0xc000
    }
    it should "transpose to hex string" in {
        transposeBoard(0xC0000000).toHexString shouldBe "c0000000"
        transposeBoard(0x30000000).toHexString shouldBe "3000000"
        transposeBoard(0x0C000000).toHexString shouldBe "c0000"
        transposeBoard(0x03000000).toHexString shouldBe "30000000"
        transposeBoard(0x00C00000).toHexString shouldBe "c00000"
        transposeBoard(0x00300000).toHexString shouldBe "30000"
        transposeBoard(0x000C0000).toHexString shouldBe "c000000"
        transposeBoard(0x00030000).toHexString shouldBe "300000"
        transposeBoard(0x0000C000).toHexString shouldBe "c000"
    }
    it should "transpose to Int 2" in {
        transposeBoard(0xF0000000) shouldBe 0xC3000000
        transposeBoard(0x03C00000) shouldBe 0x30c00000
    }
    it should "transpose String" in {
        TicTacToeInt.parse("X  X  X  ").map(x => x.transpose).get.toString() shouldBe "XXX\n...\n...\n"
        transposeBoard(0x03C00000) shouldBe 0x30c00000
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

    behavior of "TicTacToeInt"
    it should "row" in {
        val ty: Try[TicTacToeInt] = TicTacToeInt.parse("X   X   X")
        ty.map(_.row(0)) should matchPattern { case Success(16) => }
        ty.map(_.row(1)) should matchPattern { case Success(4) => }
        ty.map(_.row(2)) should matchPattern { case Success(1) => }
    }

    it should "open" in {
        val ttt = TicTacToeInt()
        ttt.open.size shouldBe stride * stride
    }

    it should "play true, 0, 0" in {
        val t0 = TicTacToeInt()
        val t1 = t0.play(xOrO = true)(0, 0)
        t1.open.size shouldBe stride * stride - 1
        t1.toString shouldBe "X..\n...\n...\n"
    }

    it should "play false, 1, 0" in {
        val t0 = TicTacToeInt()
        val t1 = t0.play(xOrO = false)(1, 0)
        t1.open.size shouldBe stride * stride - 1
        t1.toString shouldBe "...\n0..\n...\n"
    }

    it should "playX 0, 0 and play0 1, 0" in {
        val t0 = TicTacToeInt()
        val t1 = t0.playX(0, 0)
        val t2 = t1.play0(1, 0)
        t2.open.size shouldBe stride * stride - 2
        t2.toString shouldBe "X..\n0..\n...\n"
    }

    it should "parse" in {
        TicTacToeInt.parse("         ") should matchPattern { case Success(TicTacToeInt.start) => }
        val ty1 = TicTacToeInt.parse("X        ")
        ty1 should matchPattern { case Success(_) => }
        ty1.get shouldBe TicTacToeInt().playX(0, 0)
        val ty2 = TicTacToeInt.parse("X  0     ")
        ty2 should matchPattern { case Success(_) => }
        ty2.get shouldBe TicTacToeInt().playX(0, 0).play0(1, 0)
    }

    it should "not parse" in {
        TicTacToeInt.parse("") should matchPattern { case Failure(_) => }
        TicTacToeInt.parse("        ") should matchPattern { case Failure(_) => }
        TicTacToeInt.parse("          ") should matchPattern { case Failure(_) => }
    }

    it should "line row" in {
        TicTacToeInt.parse("XXX      ").get.line shouldBe Some(true)
        TicTacToeInt.parse("   XXX   ").get.line shouldBe Some(true)
        TicTacToeInt.parse("      XXX").get.line shouldBe Some(true)
        TicTacToeInt.parse("0     XXX").get.line shouldBe Some(true)
        TicTacToeInt.parse("000   X  ").get.line shouldBe Some(false)
    }

    it should "line col" in {
        TicTacToeInt.parse("X  X  X  ").get.line shouldBe Some(true)
        TicTacToeInt.parse(" X 0X  X ").get.line shouldBe Some(true)
        TicTacToeInt.parse("0 X  X  X").get.line shouldBe Some(true)
    }

    it should "line diag" in {
        TicTacToeInt.parse("XXX     0").get.line shouldBe Some(true)
        TicTacToeInt.parse("   XXX0  ").get.line shouldBe Some(true)
        TicTacToeInt.parse(" 0    XXX").get.line shouldBe Some(true)
    }
}
