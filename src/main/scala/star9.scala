package aoc2021
import scala.io.Source
import scala.math._

object Star9 {
  def getcoords(line: String): ((Int, Int), (Int, Int)) = {
    var fields = line.split("->")
    var begincoord = fields(0).split(",")
    var endcoord = fields(1).split(",")
    return ((begincoord(0).trim().toInt, begincoord(1).trim().toInt), (endcoord(0).trim().toInt, endcoord(1).trim().toInt))
  }

  def printgrid(size: Int, arr: Array[Integer]): Unit = {
    for(i <- 0 until size) {
      for(j <- 0 until size) print(arr(size * i + j) + " ")
      println()
    }
  }

  def solve(size: Int): Unit = {
    var fp = Source.fromURL(getClass.getResource("/star9_input.txt"))
    var arr = new Array[Integer](size * size)
    for(line <- fp.getLines) {
      var (start, end) = getcoords(line)
      var direction = (signum(end._1 - start._1).toInt, signum(end._2 - start._2).toInt)
      if(direction._1 == 0 || direction._2 == 0) {
        while(start._1 != end._1 || start._2 != end._2) {
          arr(size * start._2 + start._1) += 1
          start = (start._1 + direction._1, start._2 + direction._2)
        }
        arr(size * end._2 + end._1) += 1
      }
    }
    fp.close

    var nPoints = 0
    for(pt <- arr) {
      if(pt >= 2) nPoints += 1
    }
    println("Star9 Answer: %d".format(nPoints))
  }
}