package aoc2021
import scala.io.Source
import scala.collection.mutable._

object Star22 {
  def solve(): Unit = {
    var fp = Source.fromURL(getClass.getResource("/star22_input.txt"))
    var input: Array[Array[Int]] = fp.getLines.map(line => line.map(c => (c - '0').asInstanceOf[Int]).toArray).toArray
    fp.close()

    var nFlashing = 0
    var nSteps = 0
    var gridSize = input.size * input(0).size
    while(nFlashing != gridSize) {
      nSteps += 1
      nFlashing = 0
      for(row <- 0 until input.size)
        for(col <- 0 until input(row).size)
          input(row)(col) += 1

      var flashing = true
      while(flashing) {
        flashing = false
        for(row <- 0 until input.size)
          for(col <- 0 until input(row).size)
            if(input(row)(col) >= 10) {
              nFlashing += 1
              input(row)(col) = -100
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
    println("Star22 Answer: %d".format(nSteps))
  }
}