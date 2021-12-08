package aoc2021
import scala.io.Source
import util.control.Breaks._

object Star6 {
  def solve(width: Int): Unit = {
    var fp = Source.fromURL(getClass.getResource("/star6_input.txt"))
    var numList = fp.getLines.map(Integer.parseInt(_, 2)).toArray
    fp.close
    var currList = numList
    breakable { for(i <- width - 1 to 0 by -1) {
      var nOnes = 0
      for(n <- currList) nOnes += n >> i & 1
      if(nOnes < currList.size - nOnes) currList = currList.filter(x => (x & 1 << i) == 0).toArray
      else currList = currList.filter(x => (x & 1 << i) != 0).toArray
      if(currList.size <= 1) break
    }}
    assert(currList.size == 1)
    var oxygen = currList(0)
    currList = numList
    breakable { for(i <- width - 1 to 0 by -1) {
      var nOnes = 0
      for(n <- currList) nOnes += n >> i & 1
      if(nOnes < currList.size - nOnes) currList = currList.filter(x => (x & 1 << i) != 0).toArray
      else currList = currList.filter(x => (x & 1 << i) == 0).toArray
      if(currList.size <= 1) break
    }}
    assert(currList.size == 1)
    var scrubber = currList(0)
    println("Star6 Answer: %d".format(scrubber * oxygen))
  }
}