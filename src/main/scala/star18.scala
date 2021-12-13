package aoc2021
import scala.io.Source
import scala.collection.mutable._
import scala.util.Sorting

object Star18 {
  def getDigit(c: Char): Int = {
    return c.toInt - '0'.toInt
  }

  def debugMutGrid(mutGrid: Array[Array[Int]]): Unit = {
    for(row <- mutGrid) {
      for(i <- row) {
        print("%2d ".format(i))
      }
      println()
    }
    println()
  }

  def solveBasinRecurse(mutGrid: Array[Array[Int]], point: (Int, Int)): Int = {
    var size = 1
    val pointVal = mutGrid(point._1)(point._2)
    mutGrid(point._1)(point._2) = -1
    if(point._1 + 1 < mutGrid.size && mutGrid(point._1 + 1)(point._2) != 9 && pointVal < mutGrid(point._1 + 1)(point._2)) {
      size += solveBasinRecurse(mutGrid, (point._1 + 1, point._2))
    }
    if(point._1 > 0 && mutGrid(point._1 - 1)(point._2) != 9 && pointVal < mutGrid(point._1 - 1)(point._2)) {
      size += solveBasinRecurse(mutGrid, (point._1 - 1, point._2))
    }
    if(point._2 + 1 < mutGrid(point._1).size && mutGrid(point._1)(point._2 + 1) != 9 && pointVal < mutGrid(point._1)(point._2 + 1)) {
      size += solveBasinRecurse(mutGrid, (point._1, point._2 + 1))
    }
    if(point._2 > 0 && mutGrid(point._1)(point._2 - 1) != 9 && pointVal < mutGrid(point._1)(point._2 - 1)) {
      size += solveBasinRecurse(mutGrid, (point._1, point._2 - 1))
    }
    return size
  }
  def solveBasin(grid: Array[String], point: (Int, Int)): Int = {
    var mutGrid = grid.map(x => x.map(c => getDigit(c)).toArray).toArray
    return solveBasinRecurse(mutGrid, point)
  }

  def solve(): Unit = {
    var fp = Source.fromURL(getClass.getResource("/star18_input.txt"))
    var tubesGrid = fp.getLines.map(x => x.trim()).toArray
    fp.close

    var riskLevels: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)]()
    for(i <- 0 until tubesGrid.size) {
      for(j <- 0 until tubesGrid(i).size) {
        val center = getDigit(tubesGrid(i)(j))
        if((i <= 0 || center < getDigit( tubesGrid(i - 1)(j)) )
            && (i + 1 >= tubesGrid.size || center < getDigit( tubesGrid(i + 1)(j) ))
            && (j <= 0 || center < getDigit( tubesGrid(i)(j - 1) ))
            && (j + 1 >= tubesGrid(i).size || center < getDigit( tubesGrid(i)(j + 1) ))) {
          riskLevels += new Tuple2(i, j)
        }
      }
    }

    var basinSizes = riskLevels.map(x => solveBasin(tubesGrid, x)).toArray
    Sorting.quickSort(basinSizes)
    var end = basinSizes.size - 1
    var product = 1
    for(i <- 0 until 3) { product *= basinSizes(end - i) }
    println("Star18 Answer: %d".format(product))
  }
}