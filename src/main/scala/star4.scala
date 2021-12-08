package aoc2021
import scala.io.Source

object Star4 {
  def solve(): Unit = {
    var x = 0
    var y = 0
    var aim = 0
    var fp = Source.fromURL(getClass.getResource("/star4_input.txt"))
    for(line <- fp.getLines) {
      var params = line.split(" ")
      var magnitude = params(1).toInt
      params(0) match {
        case "forward" => { x += magnitude; y += magnitude * aim }
        case "down" => aim += magnitude
        case "up" => aim -= magnitude
      }
    }
    fp.close
    println("Star4 Answer: %d".format(x * y))
  }
}