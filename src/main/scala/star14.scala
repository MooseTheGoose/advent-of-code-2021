package aoc2021
import scala.io.Source
import scala.math._

object Star14 {
    def crabFormula(x: Long): Long = {
        return x * (x + 1) / 2
    }
    def solve(): Unit = {
        var fp = Source.fromURL(getClass.getResource("/star14_input.txt"))
        var crabs = fp.getLines().next().split(",").map(x => x.trim().toInt)
        fp.close()
        var max = crabs.maxBy(i => i)
        var fuelConsumed = Long.MaxValue
        for(i <- 0 to max) {
            var newFuel: Long = crabs.map(x => crabFormula(abs(x.asInstanceOf[Long] - i))).reduce((total, part) => total + part)
            if(newFuel < fuelConsumed) fuelConsumed = newFuel
        }
        println("Star14 Answer: %d".format(fuelConsumed)) // 376685, too high
    }
}