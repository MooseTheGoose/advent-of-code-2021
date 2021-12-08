package aoc2021
import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._

object Star16 {
    def parseLine(line: String): (Array[String], Array[String]) = {
        var pair = line.split("\\|")
        return (pair(0).split(" ").map(x => x.trim()), pair(1).split(" ").map(x => x.trim()))
    }

    def discoverOutput(input: Array[String], output: Array[String]): Int = {
      var outputNumber = 0
      var wirings = new Array[String](10)


      /* Find numbers based on unique length */
      for(i <- input) {
        /* We can determine 1 by finding the string with 2 wires. */
        /* We can determine 7 by finding the string with 3 wires. */ 
        /* We can determine 4 by finding the string with 4 wires. */
        /* We can determine 8 by finding the string with 7 wires. */         
        if(i.length() == 2) {
          wirings(1) = i
        } else if(i.length() == 3) {
          wirings(7) = i
        } else if(i.length() == 4) {
          wirings(4) = i
        } else if(i.length() == 7) {
          wirings(8) = i
        }
      }

      /* From this, we can determine 6 and find C, since 
       * it's the only number with 6 wires that contains f, but not c
       * 4 wires are subset of 9 wires
       * 0 wires fit none of the above categories
       */
      for(i <- input) {
          if(i.length() == 6) {
              if(!i.contains(wirings(1)(0)) || !i.contains(wirings(1)(1))) wirings(6) = i
              else if(wirings(4).filter(c => i.contains(c)).size == 4) wirings(9) = i
              else wirings(0) = i
          }
      }
      /*
       * From this, we can determine 5, 3, and 2, since
       * 5 shares all wires in common with 6,
       * 7 shares all wires in common with 3,
       * 3 shares 4 in common with 6.
       */
      for(i <- input) {
          if(i.length() == 5) {
              if(wirings(6).filter(c => i.contains(c)).size == 5) wirings(5) = i
              else if(wirings(7).filter(c => i.contains(c)).size == 3) wirings(3) = i
              else wirings(2) = i
          }
      }

      var outputDigits = 0
      for(o <- output) {
          outputDigits *= 10
          breakable { for(j <- 0 until wirings.size) {
              if(wirings(j).size == o.size && wirings(j).map(x => o.contains(x)).reduce((x, y) => x && y)) { 
                outputDigits += j
                break
              }
          }}
      }
      return outputDigits
    } 

    def solve() {
        var fp = Source.fromURL(getClass.getResource("/star16_input.txt"))
        var sum = 0
        for(line <- fp.getLines) {
          var (input, output) = parseLine(line)
          var digits = discoverOutput(input, output)
          sum += digits
        }
        fp.close
        println("Star16 Answer: %d".format(sum))
    }
}