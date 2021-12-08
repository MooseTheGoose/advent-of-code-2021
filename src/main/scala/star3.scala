package aoc2021
import scala.io.Source

object Star3 {
  def solve(): Unit = {
    var x = 0
    var y = 0
    var fp = Source.fromURL(getClass.getResource("/star3_input.txt"))
    for(line <- fp.getLines) {
      var params = line.split(" ")
      var magnitude = params(1).toInt
      params(0) match {
        case "forward" => x += magnitude
        case "down" => y += magnitude
        case "up" => y -= magnitude
      }
    }
    fp.close
    println("Star3 Answer: %d".format(x * y))
  }
}