package aoc2021
import scala.io.Source

object Star17 {
  def getDigit(c: Char): Int = {
    return c.toInt - '0'.toInt
  }

  def solve() {
    var fp = Source.fromURL(getClass.getResource("/star17_input.txt"))
    var tubesGrid = fp.getLines.map(x => x.trim()).toArray
    fp.close

    var riskLevelSum = 0
    for(i <- 0 until tubesGrid.size) {
      for(j <- 0 until tubesGrid(i).size) {
        val center = getDigit(tubesGrid(i)(j))
        if((i <= 0 || center < getDigit( tubesGrid(i - 1)(j)) )
            && (i + 1 >= tubesGrid.size || center < getDigit( tubesGrid(i + 1)(j) ))
            && (j <= 0 || center < getDigit( tubesGrid(i)(j - 1) ))
            && (j + 1 >= tubesGrid(i).size || center < getDigit( tubesGrid(i)(j + 1) ))) {
          riskLevelSum += (center + 1)
        }
      }
    }
    println("Star17 Answer: %d".format(riskLevelSum))
  }
}