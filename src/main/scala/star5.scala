package aoc2021
import scala.io.Source

object Star5 {
  def solve(width: Int): Unit = {
    var nEntries = 0
    var gamma = 0
    var epsilon = 0
    var numOnes = Array.fill(width) { 0 }
    var fp = Source.fromURL(getClass.getResource("/star5_input.txt"))
    for(line <- fp.getLines) {
      val trimmed = line.trim()
      assert(trimmed.length() == width)
      for(i <- 0 until width) {
        if(trimmed.charAt(i) == '1') numOnes(i) += 1
      }
      nEntries += 1
    }
    fp.close
    for(i <- 0 until width) {
      gamma <<= 1
      epsilon <<= 1
      if(nEntries - numOnes(i) < numOnes(i)) gamma |= 1
      else epsilon |= 1   
    }
    println("Star5 answer: %d".format(gamma * epsilon))
  }
}