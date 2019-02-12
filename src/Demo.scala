package src

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.canvas.Canvas
import scalafx.scene.Scene
import java.io.BufferedReader
import java.io.FileReader
import scalafx.scene.input.KeyEvent
import scalafx.animation.AnimationTimer
import scalafx.scene.input.MouseEvent
import scalafx.scene.input.KeyCode
import java.awt.Robot
import scalafx.scene.paint.Color
import scalafx.scene.Cursor


object Demo extends JFXApp {
  
  private var walls = Vector[Line]()
  
  val screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
  val screenHeight = screenSize.getHeight().toInt
  val screenWidth = screenSize.getWidth().toInt
  
  val windowWidth = 1280
  val windowHeight = 720
  
  //Could be used to calculate the remainder for negative numbers
  def mod(m: Double, n: Double) = {
    if(m < 0) m % n + n
    else m % n
  }
  
//  val frameRate = 60 
  val fov = 100 //Field of view in degrees
  val fovInRadians = math.toRadians(fov) //Converted to radians
  val sensitivity = 5 //Determines how much moving the mouse turns the camera. 1 is very low, 10 is very high
  
  private var heading = math.Pi / 2
  
  
  val reader = new BufferedReader(new FileReader("c:/users/mylly/onedrive/työpöytä/kentät/map.txt"))
//  val reader = new BufferedReader(new FileReader("src/map.txt"))
  
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
        walls = walls ++: Vector(new Line(vecs(0), vecs(1)), new Line(vecs(1), vecs(2)), new Line(vecs(2), vecs(3)), new Line(vecs(3), vecs(0)))
      }
  }}
  
  var wallsToRemove = Vector[Line]()
  
  //Remove walls that are covered by other walls and can thus never be seen to reduce performance issues,
  //if there are 
  for(wall <- walls) {
    if(walls.exists{another => another.v1.x == wall.v2.x && another.v1.y == wall.v2.y && 
                               another.v2.x == wall.v1.x && another.v2.y == wall.v1.y }) {
      wallsToRemove = wallsToRemove :+ wall
    }
  }
  walls = walls.filterNot(wall => wallsToRemove.contains(wall))
  println(walls.size)
  
  private var yLoc = arr.indices.filter(row => arr(row).exists(_ == '.')).min
  private var xLoc = arr(yLoc).indexOf('.')
  private var location = new Vec(xLoc + 0.5, -yLoc - 0.5)
  
  
  val canvas = new Canvas(windowWidth, windowHeight)
  val gc = canvas.graphicsContext2D
  var color = new Color("blue")
  gc.setFill(color)
  color = new Color(0, 0, 0)
  
  
  def turn(dx: Double) = {
    this.heading += dx * sensitivity * fovInRadians
    if(this.heading < 0) {
      this.heading += 2 * math.Pi
    } else if(this.heading > 2 * math.Pi) {
      this.heading -= 2 * math.Pi
    }
  }
  def moveForward(elapsedTime: Double) = move(math.sin(this.heading), math.cos(this.heading), elapsedTime)
  def moveRight(elapsedTime: Double)   = move(math.cos(this.heading), -math.sin(this.heading), elapsedTime)
  def moveLeft(elapsedTime: Double)    = move(-math.cos(this.heading), math.sin(this.heading), elapsedTime)
  def moveBack(elapsedTime: Double)    = move(-math.sin(this.heading), -math.cos(this.heading), elapsedTime)
  
  def move(xChange: Double, yChange: Double, elapsedTime: Double) = {
    //In case the player is moving in two directions at once, it won't look like the player is moving faster
    val coEfficient = {
      if((wPressed && dPressed) || (wPressed && aPressed) || (aPressed && sPressed) || (sPressed && dPressed)) 3 * elapsedTime / math.sqrt(2)
      else 3 * elapsedTime
    }
    val newXLocation = new Vec(this.location.x + coEfficient * xChange, this.location.y)
    if(walls.forall (wall => new Line(this.location, newXLocation).lineIntersect(wall).isEmpty )) this.location = newXLocation
    val newYLocation = new Vec(this.location.x, this.location.y + coEfficient * yChange)
    if(walls.forall (wall => new Line(this.location, newYLocation).lineIntersect(wall).isEmpty )) this.location = newYLocation
  }
  
  def paint() = {
    var rectangles = Vector[Rectangle]() //All pieces of wall that will be drawn on the screen
    //For excluding the walls which are outside the current fov, TODO
//    val fovLeftBoundary = heading - fovInRadians / 2
//    val fovRightBoundary = heading + fovInRadians / 2
//    val wallsInsideFov = walls.filter { wall =>
//      (wall.v1.angle(this.location, heading) > fovLeftBoundary && wall.v1.angle(this.location, heading) < fovRightBoundary) ||
//      (wall.v2.angle(this.location, heading) > fovLeftBoundary && wall.v2.angle(this.location, heading) < fovRightBoundary)
//    }
    for(x <- 0 until windowWidth) { //Go through each ray
      //The angle of each ray depends on the field of view and the width of the window
      val rayHeading = heading + (x - windowWidth / 2) * fovInRadians / windowWidth 
      //Create a ray from current location to the direction of rayHeading
      val ray = new Line(location, new Vec(location.x + 100 * math.sin(rayHeading), location.y + 100 * math.cos(rayHeading)))
      //Contains all pieces of walls this ray intersects
      var intersections = Vector[Rectangle]() 
      //For a ray, check which walls it intersects
      for(wall <- walls) { 
        val intersection = ray.lineIntersect(wall)
        intersection match {
          case Some(intersection) => {
            val distance = new Vec(intersection.x - ray.v1.x, intersection.y - ray.v1.y).length
            val rectHeight = (1.5 * windowHeight / distance / fovInRadians / math.cos(heading - rayHeading)).toInt
            //Adjust 
            color = new Color((255 / (0.3 * distance + 1)).toInt, 0, 0)//.deriveColor(1, 1, 1 / (0.3 * distance + 0.1), 1)
            intersections = intersections :+ new Rectangle(x, distance, rectHeight, color)
          }
          case None => 
        }
      }
      
      if(intersections.nonEmpty) {
        rectangles = rectangles :+ intersections.minBy(_.distance)
      }
      
    }
    //Clear the screen from the last frame 
    gc.clearRect(0, 0, canvas.getWidth(), canvas.getHeight())
    gc.setFill("gray")
    //Set the background to gray
    gc.fillRect(0, 0, windowWidth, windowHeight)
    //Draw the walls
    rectangles.foreach { r => 
        gc.setFill(r.color)
        gc.fillRect(r.screenPosition, windowHeight / 2 - r.height / 2, 1, r.height) 
    }
  }
  var fps = 0
  private var previousTime: Long = 0
  val timer = AnimationTimer(t => { 
    val elapsedTime = (t - previousTime) / 1000000000.0 //The elapsed time in seconds
    previousTime = t
    fps = (1 / elapsedTime).toInt
    if(wPressed) moveForward(elapsedTime)
    if(aPressed) moveLeft(elapsedTime)
    if(sPressed) moveBack(elapsedTime)
    if(dPressed) moveRight(elapsedTime)
    paint() 
    gc.setFill(new Color("white"))
    gc.fillText(fps.toString, 10, 10, 100)
    })
  timer.start()
  
  
  stage = new JFXApp.PrimaryStage {
    title = "3D-visualisation"
    scene = new Scene(windowWidth, windowHeight) {
      content = canvas
      canvas.cursor = Cursor.None
    }
  }
  stage.setFullScreen(true)
  
  val robot = new Robot
  
  canvas.setOnMouseMoved{e => {
    val dx = e.sceneX - windowWidth / 2
    turn(dx / 2000)
    robot.mouseMove(screenWidth / 2, screenHeight / 2)
  }}

  private var wPressed = false
  private var aPressed = false
  private var sPressed = false
  private var dPressed = false
  
  canvas.onKeyPressed = (e: KeyEvent) => {
    e.code match {
      case KeyCode.W => wPressed = true
      case KeyCode.A => aPressed = true
      case KeyCode.S => sPressed = true        
      case KeyCode.D => dPressed = true
      case _ => 
    }
  }
  canvas.onKeyReleased = (e: KeyEvent) => {
    e.code match {
      case KeyCode.W => wPressed = false
      case KeyCode.A => aPressed = false
      case KeyCode.S => sPressed = false        
      case KeyCode.D => dPressed = false
      case _ =>
    }
  }
  canvas.requestFocus()
    
}