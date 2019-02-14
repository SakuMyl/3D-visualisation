package src

import java.io.BufferedReader
import java.io.FileReader

class World(textFile: String) {
  
  private var walls = Vector[Wall]()
  
  def getWalls = this.walls
  
  val reader = new BufferedReader(new FileReader(textFile))
  
  var lineCursor = 0
  var charCursor = 0
  var line = reader.readLine()
  var arr = Array[Array[Char]]()
  while(line != null && line.trim.nonEmpty) {
    charCursor = 0
    arr = arr :+ line.toCharArray()
    for(c <- line) {
      arr(lineCursor)(charCursor) = c
      charCursor += 1
    }
    line = reader.readLine()
    lineCursor += 1
  }
  val mapWidth = charCursor + 2
  val mapHeight = lineCursor + 2
  //Padding with walls
  arr = arr.map(row => '#' +: row :+ '#')
  val pad = ("#" * mapWidth).toCharArray()
  arr = pad +: arr :+ pad
  
  arr.indices.foreach{rowIndex => arr(rowIndex).indices.foreach{charIndex =>
    if(arr(rowIndex)(charIndex) == '#') {
        val vecs = Vector(new Vec(charIndex, -rowIndex), new Vec(charIndex + 1, -rowIndex), 
                          new Vec(charIndex + 1, -rowIndex - 1), new Vec(charIndex, -rowIndex - 1))
        walls = walls ++: Vector(new Wall(vecs(0), vecs(1)), new Wall(vecs(1), vecs(2)), new Wall(vecs(2), vecs(3)), new Wall(vecs(3), vecs(0)))
      }
  }}
  
  var wallsToRemove = Vector[Wall]()
  
  //Remove walls that are covered by other walls and can thus never be seen to reduce performance issues,
  //if there are 
  for(wall <- walls) {
    val otherWalls = walls.filter(another => another != wall)
    if(otherWalls.exists(another => another.equals(wall))) {
      wallsToRemove = wallsToRemove :+ wall
    }
  }
  walls = walls.filterNot(wall => wallsToRemove.contains(wall))
  
  //First row and column with no wall is given as the player's location
  private var yLoc = arr.indices.filter(row => arr(row).exists(_ == '.')).min
  private var xLoc = arr(yLoc).indexOf('.')
  val player = new Player(new Vec(xLoc + 0.5, -yLoc - 0.5), 0, this)
 println(walls.size) 
}