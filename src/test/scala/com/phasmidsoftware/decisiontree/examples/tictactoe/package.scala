package com.phasmidsoftware.decisiontree.examples

import com.phasmidsoftware.util.{Loggable, Loggables}

package object tictactoe {

    type Cell = Option[Boolean]

    // CONSIDER using Row(x: Int) extends AnyVal
    type Row = Int

    type RowWithMask = (Row, Int)

    implicit class Outcome(x: Option[Int]) {

        import com.phasmidsoftware.util.Flog._

        implicit val optionLoggable: Loggable[Option[Int]] = new Loggables {}.optionLoggable[Int]

        /**
         * Combine x and y in such a way that x & None = None & x = x.
         * Some(-1) yield x without evaluating y.
         * But Some(x) & Some(y) give None if x != y otherwise Some(x).
         *
         * @param y another Option[Int]
         * @return an Option[Int]
         */
        def &(y: => Option[Int]): Option[Int] = s"$x & $y: " !! x match {
            case None => y
            case Some(-1) => x
            case Some(p) => y match {
                case None => x
        case Some(-1) => y // NOTE: do we need this case?
        case Some(q) => if (p == q) Some(p) else Some(-1)
      }
    }
  }
}
