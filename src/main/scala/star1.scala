package aoc2021
import scala.io.Source

object Star1 {
  def solve(): Unit = {
    var depth = 0xffffff
    var numIncreases = 0
    var fp = Source.fromURL(getClass.getResource("/star1_input.txt"))
    for(line <- fp.getLines) {
      var newdepth = line.toInt
      if(newdepth > depth) numIncreases += 1;
      depth = newdepth
    }
    fp.close
    println("Star1 Answer: %d".format(numIncreases));
  }
}