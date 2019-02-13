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
  
  private var walls = Vector[Wall]()
  
  val screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
  val screenHeight = screenSize.getHeight().toInt
  val screenWidth = screenSize.getWidth().toInt
  
  val windowWidth = 1280
  val windowHeight = 720
  
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
        walls = walls ++: Vector(new Wall(vecs(0), vecs(1)), new Wall(vecs(1), vecs(2)), new Wall(vecs(2), vecs(3)), new Wall(vecs(3), vecs(0)))
      }
  }}
  
  var wallsToRemove = Vector[Wall]()
  
  //Remove walls that are covered by other walls and can thus never be seen to reduce performance issues,
  //if there are 
  for(wall <- walls) {
    if(walls.filter(another => another != wall).exists{another => another.equals(wall)}) {
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
  }
  def moveForward(elapsedTime: Double) = move(math.sin(this.heading), math.cos(this.heading), elapsedTime)
  def moveRight(elapsedTime: Double)   = move(math.cos(this.heading), -math.sin(this.heading), elapsedTime)
  def moveLeft(elapsedTime: Double)    = move(-math.cos(this.heading), math.sin(this.heading), elapsedTime)
  def moveBack(elapsedTime: Double)    = move(-math.sin(this.heading), -math.cos(this.heading), elapsedTime)
  def moveFR(elapsedTime: Double)      = move(math.sin(this.heading + math.Pi / 4), math.cos(this.heading + math.Pi / 4), elapsedTime)
  def moveFL(elapsedTime: Double)      = move(math.sin(this.heading - math.Pi / 4), math.cos(this.heading - math.Pi / 4), elapsedTime)
  def moveBR(elapsedTime: Double)      = move(-math.sin(this.heading - math.Pi / 4), -math.cos(this.heading - math.Pi / 4), elapsedTime)
  def moveBL(elapsedTime: Double)      = move(-math.sin(this.heading + math.Pi / 4), -math.cos(this.heading + math.Pi / 4), elapsedTime)
  
  def move(xChange: Double, yChange: Double, elapsedTime: Double) = {
    //The multiplier 3 is here to make movement faster 
    val coEfficient = 3 * elapsedTime
    val wallsNearby = walls.filter(wall => new Vec(wall.v1.x - location.x, wall.v1.y - location.y).length < 2)
    val newXLocation = new Vec(this.location.x + coEfficient * xChange, this.location.y)
    if(wallsNearby.forall (wall => new Line(this.location, newXLocation).lineIntersect(wall).isEmpty )) this.location = newXLocation
    val newYLocation = new Vec(this.location.x, this.location.y + coEfficient * yChange)
    if(wallsNearby.forall (wall => new Line(this.location, newYLocation).lineIntersect(wall).isEmpty )) this.location = newYLocation
  }
  
  def wallWithinFov(wall: Line) = {
    def pointWithinFov(point: Vec) = {
      val headingUnitized = new Vec(math.sin(heading), math.cos(heading))
      val diff = new Vec(point.x - location.x, point.y - location.y).unitize()
      //Take the dot product of the heading and diff
      val dotProduct = headingUnitized.x * diff.x + headingUnitized.y * diff.y
      dotProduct > math.cos(fovInRadians / 2)
    }
    pointWithinFov(wall.v1) || pointWithinFov(wall.v2) || 
    wall.lineIntersect(new Line(location, new Vec(location.x + 100 * math.sin(heading), location.y + 100 * math.cos(heading)))).isDefined
  }
  
  def paint() = {
    var rectangles = Vector[Rectangle]() //All pieces of wall that will be drawn on the screen
    //Exclude the walls that are outside the current fov
    val wallsInsideFov = walls.filter(wall => wallWithinFov(wall))
    for(x <- 0 until windowWidth) { //Go through each ray
      //The angle of each ray depends on the field of view and the width of the window
      val rayHeading = heading + (x - windowWidth / 2) * fovInRadians / windowWidth 
      //Create a ray from current location to the direction of rayHeading
      val ray = new Line(location, new Vec(location.x + 100 * math.sin(rayHeading), location.y + 100 * math.cos(rayHeading)))
      //Contains all pieces of walls this ray intersects
      var intersections = Vector[Rectangle]() 
      //For a ray, check which walls it intersects
      for(wall <- wallsInsideFov) { 
        val intersection = ray.lineIntersect(wall)
        intersection match {
          case Some(intersection) => 
            val distance = new Vec(intersection.x - ray.v1.x, intersection.y - ray.v1.y).length
            //The rectangle height is divided by math.cos(heading - rayHeading) to get rid of the fisheye effect
            val rectHeight = (1.5 * windowHeight / distance / fovInRadians / math.cos(heading - rayHeading)).toInt
            //Adjust the brightness of the color according to distance
            color = new Color((255 / (0.3 * distance + 1)).toInt, 0, 0)
            intersections = intersections :+ new Rectangle(x, distance, rectHeight, color)
          case None =>
        }
      }
      //Choose the closest of the intersections 
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
    if(wPressed && !sPressed) {
      if(aPressed && !dPressed) moveFL(elapsedTime)
      else if(dPressed && !aPressed) moveFR(elapsedTime)
      else moveForward(elapsedTime)
    }
    else if(sPressed && !wPressed) {
      if(aPressed && !dPressed) moveBL(elapsedTime)
      else if(dPressed && !aPressed) moveBR(elapsedTime)
      else moveBack(elapsedTime)
    }
    else if(aPressed && !dPressed) moveLeft(elapsedTime)
    else if(dPressed && !aPressed) moveRight(elapsedTime)
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