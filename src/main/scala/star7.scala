package aoc2021
import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._

object Star7 {
  def solve(): Unit = {
    var fp = Source.fromURL(getClass.getResource("/star7_input.txt"))
    var boards = new ArrayBuffer[Byte]()
    var lines = fp.getLines
    var numbers: Array[Byte] = lines.next().split(",").filter(x => x.length() > 0).map(x => x.toInt.asInstanceOf[Byte]).toArray
    lines.next()
    for(line <- lines) boards ++= line.split(" ").filter(x => x.length() > 0).map(x => x.toInt.asInstanceOf[Byte])
    fp.close

    assert(boards.length % 25 == 0)
    var firstWin = -1
    var lastNumber = -1
    breakable { for(n <- numbers) {
      for(boardIdx <- 0 until boards.length / 25) {
        for(y <- 0 until 5) {
          for(x <- 0 until 5) {
            val currNum = boards(boardIdx * 25 + y * 5 + x)
            if(currNum == n) boards(boardIdx * 25 + y * 5 + x) = (-currNum).asInstanceOf[Byte]
          }
        }

        for(y <- 0 until 5) {
          if((0 until 5).map(x => boards(boardIdx * 25 + 5 * y + x) < 0).reduce((agg, memb) => agg && memb)) {
            firstWin = boardIdx
            lastNumber = n
            break
          }
        }
        for(x <- 0 until 5) {
          if((0 until 5).map(y => boards(boardIdx * 25 + 5 * y + x) < 0).reduce((agg, memb) => agg && memb)) {
            firstWin = boardIdx
            lastNumber = n
            break
          }
        }
      }
    }}

    assert(firstWin >= 0 && lastNumber >= 0)
    var score = 0
    for(y <- 0 until 5) {
      for(x <- 0 until 5) {
        if(boards(firstWin * 25 + 5 * y + x) >= 0) score += boards(firstWin * 25 + 5 * y + x)
      }
    }
    println("Star7 Answer: %d".format(score * lastNumber))
  }
}