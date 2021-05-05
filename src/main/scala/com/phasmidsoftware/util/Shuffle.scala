package com.phasmidsoftware.util

import java.util
import java.util.Random
import scala.collection.mutable.ListBuffer

object Shuffle {

  def apply[X](xs: Seq[X]): Seq[X] = {
    import scala.jdk.CollectionConverters._
    val listBuffer: ListBuffer[X] = new ListBuffer().appendAll(xs)
    val jList: util.List[X] = listBuffer.asJava
    java.util.Collections.shuffle(jList, new Random(3L))
    jList.asScala.toSeq
  }

}
