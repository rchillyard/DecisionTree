package com.phasmidsoftware.decisiontree.examples.tictactoe

import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToe.parseString
import com.phasmidsoftware.decisiontree.examples.tictactoe.TicTacToeOps._
import com.phasmidsoftware.decisiontree.moves.State
import TicTacToe.size
import com.phasmidsoftware.util.PriorityQueue.maxPQ
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Failure, Success, Try}

class TicTacToeSpec extends AnyFlatSpec with should.Matchers with PrivateMethodTester {

    behavior of "TicTacToeOps"
    it should "open" in {
        TicTacToeOps.open(0) shouldBe Array(0, 1, 2, 3, 4, 5, 6, 7, 8)
        TicTacToeOps.open(0x40000000) shouldBe Array(1, 2, 3, 4, 5, 6, 7, 8)
        TicTacToeOps.open(0x0C000000) shouldBe Array(0, 1, 2, 3, 4, 5, 6, 7, 8)
        TicTacToeOps.open(0x40400000) shouldBe Array(1, 2, 3, 5, 6, 7, 8)
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
    // CONSIDER skipping the toHexString part
    it should "hFlip to hex string" in {
        hFlip(0xC0000000).toHexString shouldBe "c0000"
        hFlip(0x30000000).toHexString shouldBe "30000"
        hFlip(0x0C000000).toHexString shouldBe "c000"
        hFlip(0x03000000).toHexString shouldBe "3000000"
        hFlip(0x00C00000).toHexString shouldBe "c00000"
        hFlip(0x00300000).toHexString shouldBe "300000"
        hFlip(0x000C0000).toHexString shouldBe "c0000000"
        hFlip(0x00030000).toHexString shouldBe "30000000"
        hFlip(0x0000C000).toHexString shouldBe "c000000"
    }
    // CONSIDER skipping the toHexString part
    it should "rotate to hex string" in {
        rotate(0xC0000000).toHexString shouldBe "c000000"
        rotate(0x30000000).toHexString shouldBe "300000"
        rotate(0x0C000000).toHexString shouldBe "c000"
        rotate(0x03000000).toHexString shouldBe "30000000"
        rotate(0x00C00000).toHexString shouldBe "c00000"
        rotate(0x00300000).toHexString shouldBe "30000"
        rotate(0x000C0000).toHexString shouldBe "c0000000"
        rotate(0x00030000).toHexString shouldBe "3000000"
        rotate(0x0000C000).toHexString shouldBe "c0000"
    }
    it should "transpose to Int using hFlip and rotate" in {
        rotate(hFlip(0xC0000000)) shouldBe 0xC0000000
        rotate(hFlip(0x30000000)) shouldBe 0x03000000
        rotate(hFlip(0x0C000000)) shouldBe 0x000C0000
        rotate(hFlip(0x03000000)) shouldBe 0x30000000
        rotate(hFlip(0x00C00000)) shouldBe 0xc00000
        rotate(hFlip(0x00300000)) shouldBe 0x30000
        rotate(hFlip(0x000C0000)) shouldBe 0xc000000
        rotate(hFlip(0x00030000)) shouldBe 0x300000
        rotate(hFlip(0x0000C000)) shouldBe 0xc000
        rotate(hFlip(0x44000000)) shouldBe 0x40040000

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
        transposeBoard(0x44000000) shouldBe 0x40040000
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
    it should "rowLine" in {
        rowLine(0x00) shouldBe 0
        rowLine(0x15) shouldBe 1
        rowLine(0x2A) shouldBe 2
    }
    it should "parseString" in {
        parseString("X..X..X..").board.value shouldBe 0x41040000
        parseString("XXX......").board.value shouldBe 0x54000000
        parseString("X.X......").board.value shouldBe 0x44000000
        parseString("0.0......").board.value shouldBe 0x88000000
        parseString("XX.......").board.value shouldBe 0x50000000
        parseString("00.......").board.value shouldBe 0xA0000000
        parseString(".XX......").board.value shouldBe 0x14000000
        parseString(".00......").board.value shouldBe 0x28000000
    }
    it should "rowLinePending" in {
        rowLinePending(0x00) shouldBe 0
        rowLinePending(0x11) shouldBe 1
        rowLinePending(0x22) shouldBe 2
        rowLinePending(0x14) shouldBe 1
        rowLinePending(0x28) shouldBe 2
        rowLinePending(0x05) shouldBe 1
        rowLinePending(0x0A) shouldBe 2
    }

    behavior of "TicTacToe"
    ignore should "row" in {
        val ty: Try[TicTacToe] = TicTacToe.parse("X   X   X")
        val rowMethod = PrivateMethod[Int](Symbol("row"))
        val invokeRow: Int => TicTacToe => Int = x => t => t invokePrivate rowMethod(x)
        ty.map(invokeRow(0)) should matchPattern { case Success(16) => }
        ty.map(invokeRow(1)) should matchPattern { case Success(4) => }
        ty.map(invokeRow(2)) should matchPattern { case Success(1) => }
    }

    it should "open" in {
        val ttt = TicTacToe()
        ttt.open.size shouldBe TicTacToe.size * TicTacToe.size
    }

    it should "play true, 0, 0" in {
        val t0 = TicTacToe()
        val b1: (Board, TicTacToe) = t0.play(xOrO = true)(0, 0)
        val bTs = implicitly[State[Board, TicTacToe]]
        val t1 = bTs.construct(b1, maxPQ[TicTacToe])
        t1.open.size shouldBe TicTacToe.size * TicTacToe.size - 1
        t1.render() shouldBe "\nX..\n...\n...\n (1.0)"
    }

    it should "play false, 1, 0" in {
        val t0 = TicTacToe()
        val b1 = t0.play(xOrO = false)(1, 0)
        val bTs = implicitly[State[Board, TicTacToe]]
        val t1 = bTs.construct(b1, maxPQ[TicTacToe])
        t1.open.size shouldBe TicTacToe.size * TicTacToe.size - 1
        t1.render() shouldBe "\n...\n0..\n...\n (0.0)"
    }

    it should "playX 0, 0 and play0 1, 0" in {
        val t0 = TicTacToe()
        val b1 = t0.playX(0, 0)
        val bTs = implicitly[State[Board, TicTacToe]]
        val t1 = bTs.construct(b1, maxPQ[TicTacToe])
        val b2 = t1.play0(1, 0)
        val t2 = bTs.construct(b2, maxPQ[TicTacToe])
        t2.open.size shouldBe TicTacToe.size * TicTacToe.size - 2
        t2.render() shouldBe "\nX..\n0..\n...\n (0.0)"
    }

    it should "parse" in {
        TicTacToe.parse("         ") should matchPattern { case Success(TicTacToe.start) => }
        val ty1 = TicTacToe.parse("X        ")
        ty1 should matchPattern { case Success(_) => }
        ty1.get shouldBe TicTacToe(TicTacToe().playX(0, 0))
        val ty2 = TicTacToe.parse("X  0     ")
        ty2 should matchPattern { case Success(_) => }
        //        ty2.get shouldBe TicTacToe().playX(0, 0).play0(1, 0)
    }

    it should "not parse" in {
        TicTacToe.parse("") should matchPattern { case Failure(_) => }
        TicTacToe.parse("        ") should matchPattern { case Failure(_) => }
        TicTacToe.parse("          ") should matchPattern { case Failure(_) => }
    }

    it should "win row" in {
        TicTacToe.parse("XXX      ").get.win shouldBe Some(true)
        TicTacToe.parse("   XXX   ").get.win shouldBe Some(true)
        TicTacToe.parse("      XXX").get.win shouldBe Some(true)
        TicTacToe.parse("0     XXX").get.win shouldBe Some(true)
        TicTacToe.parse("000   X  ").get.win shouldBe Some(false)
    }

    it should "win col" in {
        TicTacToe.parse("X  X  X  ").get.win shouldBe Some(true)
        TicTacToe.parse(" X 0X  X ").get.win shouldBe Some(true)
        TicTacToe.parse("0 X  X  X").get.win shouldBe Some(true)
    }

    it should "win diag" in {
        TicTacToe.parse("XXX     0").get.win shouldBe Some(true)
        TicTacToe.parse("   XXX0  ").get.win shouldBe Some(true)
        TicTacToe.parse(" 0    XXX").get.win shouldBe Some(true)
    }

    it should "peneWin" in {
        TicTacToe.from(0x44000000).peneWin shouldBe Some(true)
        TicTacToe.from(0x40040000).peneWin shouldBe Some(true)
        TicTacToe.from(0x40400000).peneWin shouldBe Some(true)
        TicTacToe.parse(".00     0").get.peneWin shouldBe Some(false)
    }

    it should "block" in {
        TicTacToe(TicTacToe.parse("0 0     0").get.play(xOrO = true)(0, 1)).block shouldBe Some(true)
        TicTacToe(TicTacToe.parse("X X     0").get.play(xOrO = false)(0, 1)).block shouldBe Some(false)
    }

    it should "heuristic 1" in {
        val z = implicitly[State[Board, TicTacToe]]
        z.heuristic(TicTacToe.parse("   XXX   ").get) shouldBe 7 // X win
        z.heuristic(TicTacToe.parse("      XXX").get) shouldBe 7 // X win
        z.heuristic(TicTacToe.parse("000   X  ").get) shouldBe 7 // 0 win
        z.heuristic(TicTacToe.parse("0 0      ").get) shouldBe 5 // 0 pending
        z.heuristic(TicTacToe.parse("   X X  0").get) shouldBe 5 // X pending
        z.heuristic(TicTacToe.parse("0     X X").get) shouldBe 5 // X pending

    }

    it should "heuristic where difference may matter" in {
        val z = implicitly[State[Board, TicTacToe]]
        z.heuristic(TicTacToe(TicTacToe.parse("         ").get.play(xOrO = true)(2, 2))) shouldBe 1 // X corner
        z.heuristic(TicTacToe(TicTacToe.parse(".       X").get.play(xOrO = false)(0, 0))) shouldBe 2 // 0 opposite corner
        z.heuristic(TicTacToe(TicTacToe.parse("0     X0.").get.play(xOrO = true)(2, 2))) shouldBe 2 // X opposite corner
        z.heuristic(TicTacToe(TicTacToe.parse("0X0   .  ").get.play(xOrO = true)(2, 0))) shouldBe 2 // X opposite corner
        z.heuristic(TicTacToe(TicTacToe.parse("         ").get.play(xOrO = true)(1, 1))) shouldBe 3 // X center
        z.heuristic(TicTacToe(TicTacToe.parse("   X X   ").get.play(xOrO = false)(1, 1))) shouldBe 3 // 0 center
        z.heuristic(TicTacToe(TicTacToe.parse(" XX      ").get.play(xOrO = true)(0, 0))) shouldBe 7 // X win
        z.heuristic(TicTacToe(TicTacToe.parse("0 0      ").get.play(xOrO = true)(0, 1))) shouldBe 6 // X block
        z.heuristic(TicTacToe(TicTacToe.parse("0     X X").get.play(xOrO = false)(2, 1))) shouldBe 6 // 0 block
        z.heuristic(TicTacToe(TicTacToe.parse("....0..XX").get.play(xOrO = false)(2, 0))) shouldBe 6 // 0 block
    }

    it should "get best play" in {
        val z = implicitly[State[Board, TicTacToe]]
        val qs: Seq[TicTacToe] = z.getStates(TicTacToe.parse(".........").get, maxPQ)
        z.heuristic(qs.sorted.reverse.head) shouldBe 3
    }

}
