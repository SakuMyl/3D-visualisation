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
import scalafx.scene.Cursor
import scalafx.scene.paint.Color
import scalafx.scene.input.KeyCodeCombination
import javafx.scene.input.KeyCombination
import scalafx.scene.text.Font


object Demo extends JFXApp {
  
  val world: World = {
    try {
      new World("src/map.txt")
    } catch {
      case e: InvalidFileException =>
        new World("src/Default map.txt")
    }
  }
  
  val player = world.player
  
  val screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
  val screenHeight = screenSize.getHeight().toInt
  val screenWidth = screenSize.getWidth().toInt
  
  private var windowWidth = 1280
  private var windowHeight = 720
  
  val renderingDistance = 20
  
  val canvas = new Canvas(windowWidth, windowHeight)
  val gc = canvas.graphicsContext2D
  val color = new Color("red")
  
  def pause() = {
    canvas.cursor = Cursor.Default
    gc.font = new Font(100)
    gc.fillText("PAUSED", 50, 100)
    gc.font = Font.default
    timer.stop()
    this.paused = true
  }
  def continue() = {
    canvas.cursor = Cursor.None
    timer.start()
    this.paused = false
  }
  private var paused = false
  
  def setFullScreen() = {
    this.windowWidth = 1280
    this.windowHeight = 720
    stage.setFullScreen(true)
  }
  
  def setSmallWindow() = {
    stage.setFullScreen(false)
    this.windowWidth = 860
    this.windowHeight = 540
    stage.setWidth(windowWidth)
    stage.setHeight(windowHeight)
    stage.centerOnScreen()
  }
  def paint() = {
    var rectangles = Vector[Rectangle]() //All pieces of wall that will be drawn on the screen
    //Exclude the walls that are outside the current fov
    val wallsInsideFov = world.getWalls.filter(wall => player.wallWithinFov(wall))
    for(x <- 0 until windowWidth) { //Go through each ray
      //The angle of each ray depends on the field of view and the width of the window
      val rayHeading = player.getHeading + (x - windowWidth / 2) * player.fov / windowWidth 
      //Create a ray from current location to the direction of rayHeading to a distance equivalent to rendering distance
      val ray = new Line(player.getLocation, new Vec(player.getLocation.x + renderingDistance * math.sin(rayHeading), 
                                                     player.getLocation.y + renderingDistance * math.cos(rayHeading)))
      //Contains all pieces of walls this ray intersects
      var intersections = Vector[Rectangle]() 
      //For a ray, check which walls it intersects
      for(wall <- wallsInsideFov) { 
        val intersection = ray.lineIntersect(wall)
        intersection match {
          case Some(intersection) => 
            val distance = (ray.v1 - intersection).length
            //The rectangle height is divided by math.cos(heading - rayHeading) to get rid of the fisheye effect
            val rectHeight = (1.5 * windowHeight / (distance * player.fov * math.cos(player.getHeading - rayHeading))).toInt
            //Adjust the brightness of the color according to distance
            val rectColor = new Color(wall.color).deriveColor(1, 1, (1 / (0.3 * distance + 1)),1)
            intersections = intersections :+ new Rectangle(x, distance, rectHeight, rectColor)
          case None =>
        }
      }
      //Choose the closest of the intersections 
      if(intersections.nonEmpty) rectangles = rectangles :+ intersections.minBy(_.distance)
      
    }
    //Clear the screen from the last frame 
    gc.clearRect(0, 0, windowWidth, windowHeight)
    val backGroundColor = new Color("gray")
    //Paint the background. Brightness of the background depends on how close to the center of the screen it is
    for(y <- 0 until windowHeight) {
      gc.setFill(backGroundColor.deriveColor(1, 1, ((y - windowHeight / 2).abs) / windowHeight.toDouble, 1))
      gc.fillRect(0, y, windowWidth, 1)
    }
    //Draw the walls
    rectangles.foreach { r => 
        gc.setFill(r.color)
        gc.fillRect(r.screenPosition, windowHeight / 2 - r.height / 2, 1, r.height) 
    }
  }
  var fps = 0
  private var previousTime: Long = 0
  var i = 0
  val timer = AnimationTimer(t => { 
    val elapsedTime = (t - previousTime) / 1000000000.0 //The elapsed time in seconds
    previousTime = t
    if(i % 10 == 0) fps = (1 / elapsedTime).toInt
    if(wPressed && !sPressed) {
      if(aPressed && !dPressed) player.moveFL(elapsedTime)
      else if(dPressed && !aPressed) player.moveFR(elapsedTime)
      else player.moveForward(elapsedTime)
    }
    else if(sPressed && !wPressed) {
      if(aPressed && !dPressed) player.moveBL(elapsedTime)
      else if(dPressed && !aPressed) player.moveBR(elapsedTime)
      else player.moveBack(elapsedTime)
    }
    else if(aPressed && !dPressed) player.moveLeft(elapsedTime)
    else if(dPressed && !aPressed) player.moveRight(elapsedTime)
    paint()
    gc.setFill(new Color("white"))
    gc.fillText(fps.toString, 10, 10, 100)
    i += 1
    })
  timer.start()
  
  
  stage = new JFXApp.PrimaryStage {
    fullScreen = true
    resizable = false
    fullScreenExitKey = KeyCombination.NO_MATCH
    title = "3D-visualisation"
    scene = new Scene {
      content = canvas
      canvas.cursor = Cursor.None
    }
  }
  val robot = new Robot
  
  canvas.setOnMouseMoved{e => {
    if(!paused) {
      val dx = e.screenX - (stage.getX() + stage.getWidth() / 2)
      player.turn(dx / 2000)
      robot.mouseMove(screenWidth / 2, screenHeight / 2)
    }
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
      case KeyCode.F => {
        if(!stage.fullScreen.value) setFullScreen()
        else setSmallWindow()
      }
      case KeyCode.P => if(paused) continue() else pause()
      case KeyCode.Escape => stage.close() 
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