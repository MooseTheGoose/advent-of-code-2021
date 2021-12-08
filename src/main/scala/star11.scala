package aoc2021
import scala.io.Source
import scala.collection.mutable._

object Star11 {
  def computeOffspring(nDays: Int, lanternFish: Int): Int = {
    var fishList = new ArrayBuffer[Int]()
    val timeToOffspring = 6
    val newbredTime = 8
    fishList += lanternFish
    for(i <- 0 until nDays) {
      val end = fishList.size
      for(j <- 0 until end) {
        fishList(j) -= 1
        if(fishList(j) < 0) {
          fishList += newbredTime
          fishList(j) = timeToOffspring
        }
      }
    }
    return fishList.size
  }

  def solve(nDays: Int): Unit = {
    var fp = Source.fromURL(getClass.getResource("/star11_input.txt"))
    var lanternFishes = fp.getLines.next().split(",").map(x => x.trim().toInt).toArray
    fp.close

    var totalFishes = 0
    for(fish <- lanternFishes) {
      totalFishes += computeOffspring(nDays, fish)
    }
    println("Star11 Answer: %d".format(totalFishes))
  }
}