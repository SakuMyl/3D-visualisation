package main.scala

import java.io.BufferedReader
import java.io.FileReader
import scalafx.scene.image.Image
import scala.io.Source

class World(textFile: String) {
  
  private var walls = Vector[Wall]()
  
  def getWalls = this.walls
  
  /*
   * The textures to be used for drawing the walls.
   */
  val path = "file:src/main/resources/textures"
  val textures = Array(
      
//      new Image(path + "/colored-stone-pavement_black.jpg"),
//      new Image(path + "/colored-stone-pavement_blue.jpg"),
//      new Image(path + "/colored-stone-pavement.jpg"),
//      new Image(path + "/RustPlain.jpg"),
//      new Image(path + "/WoodPlanks.jpg"),
    new Image(path + "/redbrick.png"),
    new Image(path + "/bluestone.png"),
    new Image(path + "/colorstone.png"),
    new Image(path + "/mossy.png"),
    new Image(path + "/purplestone.png"),
    new Image(path + "/greystone.png")
  )
      
  val reader = Source.fromFile(textFile).getLines
  
  //Keeps track of the line index
  var lineCursor = 0
  //Keeps track of the character index in a line
  var charCursor = 0
  //First line
  var line = reader.next().trim
  var length = line.length
  //The array to which the map characters will be stored
  var arr = Array[Array[Char]]()
  var done = false
  while(reader.hasNext && !done) {
    /* 
     * In case there are empty rows in the beginning of the file,
     * they will be skipped until a non-empty row is found. 
     * File reading will stop when an empty row is encountered or 
     * the file ends. 
     */
    if(line.length == length || length == 0) {
      if(length == 0) length = line.length
      try {
        /*
         * If a line is empty in the beginning of the file,
         * it will be skipped.
         */
        if(length != 0) {
          charCursor = 0
          //Append the current line to the array
          arr = arr :+ line.toCharArray()
          //Go through every character in a line
          for(c <- line) {
            /*
             * A dot means an empty space with no wall in the map
             */
            if(c == '.') {
              arr(lineCursor)(charCursor) = '.'
            }
            /*
             * c has to be a valid texture number, 
             * i.e. a number that corresponds to some
             * texture number
             */
            else if((0 to textures.size).contains(c.asDigit)) {
              arr(lineCursor)(charCursor) = c
            }
            else {
              throw new InvalidFileException("Invalid characters in file")
            }
            //Move to the next character
            charCursor += 1
          }
          //Move to the next line
          lineCursor += 1 
        }
        //Read the next line
        line = reader.next().trim
      } catch {
        case e: Exception => 
          println("Something went wrong")
          e.printStackTrace()
      }
      /*
       * In case some row with a different length is encountered,
       * an exception will be thrown.
       */
    } else if(line.length != 0){
      throw new InvalidFileException("Some rows were of different length")
      /*
       * If an empty line is encountered in the file, the reader will be closed
       */
    } else {
      done = true 
    }
    
  }
  /*
   * To make sure there is empty space in the world i.e. it isn't full of walls
   * and that the map isn't completely empty.
   */
  if(!arr.exists(_.exists(c => c == '.'))) {
    throw new InvalidFileException("No empty space found in map (only walls) or map was empty")
  }
  
  /*
   * Padding the map with walls in all sides to make sure the 
   * player's movement is limited to a closed area
   */
  arr = arr.map(row => '0' +: row :+ '0')
  val pad = ("0" * arr(0).size).toCharArray()
  arr = pad +: arr :+ pad
  
  /*
   * Create walls to the world according to the characters 
   * in the 2-dimensional array. 
   */
  def createWalls(arr: Array[Array[Char]]) = {
    arr.indices.foreach{rowIndex => arr(rowIndex).indices.foreach{charIndex =>
      if(arr(rowIndex)(charIndex) != '.') {
        //The texture number 
        val tex = arr(rowIndex)(charIndex).asDigit
        //The vertices of the walls to be created
        val vecs = Vector(new Vec(charIndex, -rowIndex), new Vec(charIndex + 1, -rowIndex), 
                          new Vec(charIndex + 1, -rowIndex - 1), new Vec(charIndex, -rowIndex - 1))
        /*
         * Create four walls in a clockwise manner 
         * according to the vertices and the texture number.
         */
        this.walls = this.walls ++: Vector(new Wall(vecs(0), vecs(1), tex), new Wall(vecs(1), vecs(2), tex), 
                                           new Wall(vecs(2), vecs(3), tex), new Wall(vecs(3), vecs(0), tex))
      }
    }}
  }
  createWalls(arr)
    
  var wallsToRemove = Vector[Wall]()
  
  /* Remove walls that are covered by other walls to increase performance.
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
  val player = new Player(new Vec(xLoc + 0.5, -yLoc - 0.5), math.Pi / 2, this)
}