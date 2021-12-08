package aoc2021
import scala.io.Source

object Star2 {
  def solve(): Unit = {
    var window = Array(0xffffff, 0xffffff, 0xffffff)
    var currWindow = 0xfffffff
    var nIncreases = 0
    var fp = Source.fromURL(getClass.getResource("/star2_input.txt"))
    for(line <- fp.getLines) {
      var newDepth = line.toInt
      for(i <- 0 until 2) window(i) = window(i + 1)
      window(2) = newDepth
      var newWindow = 0
      for(i <- 0 until 3) newWindow += window(i)
      if(currWindow < newWindow) nIncreases += 1
      currWindow = newWindow
    }
    fp.close
    println("Star2 Answer: %d".format(nIncreases))
  }
}