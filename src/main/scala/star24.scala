package aoc2021
import scala.io.Source
import scala.collection.mutable._
import scala.math._

object Star24 {
  class Graph(var name: String, var connections: ArrayBuffer[Graph] = new ArrayBuffer[Graph]) {
    def add_connection(x: Graph): Unit = {
      connections += x
    }
  }

  class Path(var root: Graph, var path: ArrayBuffer[Graph] = new ArrayBuffer[Graph]) {
    def this(r: Graph) {
      this(r, new ArrayBuffer[Graph])
      path += r
    }
    def verifyPath(path: ArrayBuffer[Graph], node: Graph): Boolean = {
      var verified = !node.name.equals("start")
      if(verified) {
        var sortedPath = path.clone()
        sortedPath += node
        sortedPath = sortedPath.filter(n => n.name(0).isLower).sortWith((x, y) => x.name.compareTo(y.name) < 0)
        var nDups = 0
        for(i <- 0 until sortedPath.size - 1)
          if(sortedPath(i).name.equals(sortedPath(i+1).name))
            nDups += 1
        verified = nDups < 2
      }
      return verified
    }
    def traverse(node: Graph): Path = {
      var newPath: Path = null

      if(verifyPath(path, node)) {
        val newNodes = new ArrayBuffer[Graph](path.size)
        path.copyToBuffer(newNodes)
        newNodes += node
        newPath = new Path(root, newNodes)
      }
      return newPath
    }
  }

  def getPaths(root: Graph): ArrayBuffer[Path] = {
    var completedPaths = new ArrayBuffer[Path]()
    var currentPaths = new ArrayBuffer[Path]()
    currentPaths += new Path(root)
    while(currentPaths.size > 0) {
      var currPath = currentPaths.remove(currentPaths.size - 1)
      var currPos = currPath.path(currPath.path.size - 1)
      if(!currPos.name.equals("end")) {
        for (child <- currPos.connections) {
          val newPath = currPath.traverse(child)
          if (newPath != null) currentPaths += newPath
        }
      } else {
        completedPaths += currPath
      }
    }
    return completedPaths
  }

  def solve(): Unit = {
    var fp = Source.fromURL(getClass.getResource("/star23_input.txt"))
    var graphDict = new HashMap[String, Graph]()
    for(line <- fp.getLines) {
      var fields = line.split("-");
      for(field <- fields.map(f => f.trim()))
        if(!graphDict.contains(field))
          graphDict(field) = new Graph(field)
      graphDict(fields(0)).add_connection(graphDict(fields(1)))
      graphDict(fields(1)).add_connection(graphDict(fields(0)))
    }

    var paths = getPaths(graphDict("start"))
    println("Star24 answer: %d".format(paths.size))
  }
}