package aoc2021
import scala.io.Source
import scala.collection.mutable._

object Star21 {
  def solve(nSteps: Int): Unit = {
    var fp = Source.fromURL(getClass.getResource("/star21_input.txt"))
    var input: Array[Array[Int]] = fp.getLines.map(line => line.map(c => (c - '0').asInstanceOf[Int]).toArray).toArray
    fp.close()

    var nFlashing = 0
    for(i <- 0 until nSteps) {
      for(row <- 0 until input.size)
        for(col <- 0 until input(row).size)
          input(row)(col) += 1

      var flashing = true
      while(flashing) {
        flashing = false
        for(row <- 0 until input.size)
          for(col <- 0 until input(row).size)
            if(input(row)(col) >= 10) {
              input(row)(col) = -100
              nFlashing += 1
              flashing = true
              for(i <- -1 to 1)
                for(j <- -1 to 1)
                  if(i != 0 || j != 0) {
                    val neighborRow = row + i
                    val neighborCol = col + j
                    if(neighborRow >= 0 && neighborRow < input.size
                    && neighborCol >= 0 && neighborCol < input(row).size)
                      input(neighborRow)(neighborCol) += 1
                  }
            }
      }

      for(row <- 0 until input.size)
        for(col <- 0 until input(row).size)
          if(input(row)(col) < 0) input(row)(col) = 0
    }

    println("Star21 Answer: %d".format(nFlashing))
  }
}