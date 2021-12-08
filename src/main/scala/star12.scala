package aoc2021
import scala.io.Source
import scala.collection.mutable._

object Star12 {
  val newbredInit = 8
  val newTimer = 6

  def computeOffspring(nDays: Int, lanternFish: Int): Int = {
    return 0
  }

  /*
   *  Compute lookup table for how many fish
   *  will be present when the laternfish has
   *  an internal timer of 0 and n days left
   */
  def computeLookupTable(nDays: Int): Array[Long] = {
    var lut = new Array[Long](nDays + 1)

    for(i <- 0 until lut.size) {
      var totalFish: Long = 1
      var daysLeft = i
      while(daysLeft > 0) {
        daysLeft -= 1
        if(daysLeft > newbredInit) totalFish += lut(daysLeft - newbredInit)
        else totalFish += 1
        daysLeft -= newTimer
      }
      lut(i) = totalFish
    }
    return lut
  }

  def solve(nDays: Int): Unit = {
    var fp = Source.fromURL(getClass.getResource("/star12_input.txt"))
    var lanternFishes = fp.getLines.next().split(",").map(x => x.trim().toInt).toArray
    fp.close
    //var lanternFishes = "3,4,3,1,2".split(",").map(x => x.trim().toInt).toArray
    var totalFishes: Long = 0
    var fishLut = computeLookupTable(nDays)
    for(fish <- lanternFishes) {
      if(nDays - fish > 0) totalFishes += fishLut(nDays - fish)
      else totalFishes += 1
    }
    println("Star12 Answer: %d".format(totalFishes))
  }
}