package src

import java.io.BufferedReader
import java.io.FileReader
import scalafx.scene.image.Image

class World(textFile: String) {
  
  private var walls = Vector[Wall]()
  
  def getWalls = this.walls
  
  val reader = new BufferedReader(new FileReader(textFile))
  
  var lineCursor = 0
  var charCursor = 0
  var line = reader.readLine().trim
  var length = line.length
  var arr = Array[Array[Char]]()
  var done = false
  while(!done) {
    /* 
     * In case there are empty rows in the beginning of the file,
     * they will be skipped until a non-empty row is found. 
     * File reading will stop when an empty row is encountered or 
     * the file ends.
     */
    if(line.length == length || length == 0) {
      if(length == 0) length = line.length
      try {
        if(length != 0) {
          charCursor = 0
          arr = arr :+ line.toCharArray()
          for(c <- line) {
            if(c == '.') {
              arr(lineCursor)(charCursor) = '.'
            }
            else if((0 to 7).contains(c.asDigit)) {
              arr(lineCursor)(charCursor) = c
            }
            else {
              throw new InvalidFileException("Invalid characters in file")
            }
            charCursor += 1
          }
          lineCursor += 1 
        }
        line = reader.readLine().trim
      } catch {
        case e: NullPointerException =>
          done = true
          reader.close()
      }
    } else if(line.length != 0){
      throw new InvalidFileException("Some rows were of different length")
    } else {
      done = true 
      reader.close()
    }
    
  }
  /*
   * To make sure there is empty space in the world i.e. it isn't full of walls
   * and that the map isn't completely empty.
   */
  if(arr.forall(_.forall(c => c != '.'))) throw new InvalidFileException("No empty space found in map (only walls) or map was empty")
  
  //Map width and height including padding 
  val mapWidth = charCursor + 2
  val mapHeight = lineCursor + 2
  /*
   * Padding the map with walls in all sides to make sure the 
   * player's movement is limited to a closed area
   */
  arr = arr.map(row => '0' +: row :+ '0')
  val pad = ("0" * mapWidth).toCharArray()
  arr = pad +: arr :+ pad
  
  /*
   * Create walls to the world according to the characters 
   * in the 2-dimensional array. 
   */
  arr.indices.foreach{rowIndex => arr(rowIndex).indices.foreach{charIndex =>
    if(arr(rowIndex)(charIndex) != '.') {
        val tex = arr(rowIndex)(charIndex).asDigit
        val vecs = Vector(new Vec(charIndex, -rowIndex), new Vec(charIndex + 1, -rowIndex), 
                          new Vec(charIndex + 1, -rowIndex - 1), new Vec(charIndex, -rowIndex - 1))
        walls = walls ++: Vector(new Wall(vecs(0), vecs(1), tex), new Wall(vecs(1), vecs(2), tex), new Wall(vecs(2), vecs(3), tex), new Wall(vecs(3), vecs(0), tex))
      }
  }}
  
  var wallsToRemove = Vector[Wall]()
  
  /* Remove walls that are covered by other walls to reduce performance issues,
   * If two walls have the same location, they are always blocked by other walls
   * and can thus never be seen by the player. 
   */
  for(wall <- walls) {
    val otherWalls = walls.filter(another => another != wall)
    if(otherWalls.exists(another => another.equals(wall))) {
      wallsToRemove = wallsToRemove :+ wall
    }
  }
  walls = walls.filterNot(wall => wallsToRemove.contains(wall))
  
  /*
   * First row and column with no wall in it is given as the 
   * player's location to prevent getting stuck inside a wall.
   */
  private var yLoc = arr.indices.filter(row => arr(row).exists(_ == '.')).min
  private var xLoc = arr(yLoc).indexOf('.')
  val player = new Player(new Vec(xLoc + 0.5, -yLoc - 0.5), 0, this)
}