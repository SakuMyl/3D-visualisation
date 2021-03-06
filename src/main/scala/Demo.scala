package main.scala

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.canvas.Canvas
import scalafx.scene.Scene
import scalafx.scene.input.{KeyCode,KeyEvent}
import javafx.scene.input.KeyCombination
import scalafx.animation.AnimationTimer
import scalafx.scene.Cursor
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, TextAlignment}
import scalafx.scene.control.Alert
import Alert.AlertType
import scalafx.scene.image.{Image, WritableImage}
import javafx.stage.Screen

object Demo extends JFXApp.PrimaryStage {
  
  //Get the width and height of the screen
  val bounds = Screen.getPrimary().getBounds()
  private var windowWidth = bounds.getWidth().toInt
  private var windowHeight = bounds.getHeight().toInt
  
  /*
   * The canvas to draw graphics on
   */
  val canvas = new Canvas(windowWidth, windowHeight)
  val gc = canvas.graphicsContext2D
  
  //The stage takes up all of the screen initially
  fullScreen = true
  //The window is set to be unresizable to avoid messing up the canvas
  resizable = false
  //Avoid messing with the default exit key
  fullScreenExitKey = KeyCombination.NO_MATCH
  scene = new Scene {
    content = canvas
    //Remove the distracting cursor on the window.
    canvas.cursor = Cursor.None
  }
  
  /*
   * The world is created from the text file.
   * If there is something wrong with the file,
   * an alert pops up notifying about the error
   * and the default map is loaded instead.
   */
  val world: World = {
    try {
      new World("src/main/resources/maps/map.txt")
    } catch {
      case e: InvalidFileException =>
        new Alert(AlertType.Error) {
          headerText = "Error when loading map, default map loaded instead"
          contentText = e.getMessage
        }.showAndWait() 
        new World("src/main/resources/maps/Default map.txt")
    }   
  }
  
  val player = world.player
  
  val renderingDistance = 25
  
  
      
  /*
   * Allows the player to "pause" the demo. 
   * Frees the cursor from the robot controlling
   * it so the player can easily exit the window 
   * and continue later.
   */
  def pause() = {
    canvas.cursor = Cursor.Default
    paint()
    //Stop the timer so that the image stays still and doesn't respond to mouse movements
    timer.stop()
    //Font size and color are set so that it's clearly visible that the game is paused
    gc.setFill("white")
    gc.setTextAlign(TextAlignment.Center)
    gc.font = new Font(100)
    gc.fillText("PAUSED", this.getWidth() / 2, this.getHeight() / 2)
    gc.font = Font.default
    this.paused = true
  }
  
  /*
   * Allows continuing after a pause
   */
  def continue() = {
    canvas.cursor = Cursor.None
    this.paused = false
    timer.start()
  }
  
  private var paused = false
  
  /*
   * Allows setting the window back to fullscreen
   */
  def setFullScreen() = {
    this.windowWidth = bounds.getWidth().toInt
    this.windowHeight = bounds.getHeight().toInt
    this.setResizable(true)
    this.setWidth(windowWidth)
    this.setHeight(windowHeight)
    this.fullScreen = true
    //Avoid messing up the location of the "paused" text in case the demo is paused
    if(paused) {
      continue()
      pause()
    }
  }
  
  /*
   * Sets the window from fullscreen to a smaller window
   */
  def setSmallWindow() = {
    this.fullScreen = false
    this.resizable = false
    /*
     * Change the window width and height so 
     * all the walls scale accordingly
     */
    this.windowWidth /= 2
    this.windowHeight /= 2
    this.setWidth(windowWidth)
    this.setHeight(windowHeight)
    /*
     * The window is center on the screen
     * by default, but it can be moved 
     */
    this.centerOnScreen()
    /*
     * The demo has to paused and continued so that the
     * "PAUSED" text will reposition itself to the center 
     * of the screen
     */
    if(paused) {
      continue()
      pause()
    }
  }
  
  /*
   * Create a new image from an existing one with a different 
   * brightness level according to "distance". That is, the 
   * distance from the player to a point of a wall. This is 
   * necessary for creating multiple brightness levels for 
   * the textures.
   */
  def getImageWithBrightness(image: Image, distance: Double) = {
    val brightnessCoefficient = 1 / (0.2 * distance + 1)
    val width = image.getWidth().toInt
    val height = image.getHeight().toInt
    val pixelReader = image.getPixelReader()
    val out = new WritableImage(width, height)
    val pixelWriter = out.getPixelWriter()
    for(i <- 0 until width) {
      for(j <- 0 until height) {
        pixelWriter.setColor(i, j, pixelReader.getColor(i, j).deriveColor(1, 1, brightnessCoefficient, 1))
      }
    }
    out
  }
  /* 
   * A precalculated array of multiple brightness levels for each texture.
   */
  val textureStripes: Array[Array[Image]] = Array.tabulate(world.textures.size)(tex => 
        Array.tabulate(10 * renderingDistance  + 1)(distance =>
          getImageWithBrightness(world.textures(tex), 0.1 * distance)))
          
  /*
   * An alert notifying the user of the controls
   * pops up before the app window.
   */
  new Alert(AlertType.Information) {
      headerText = "Controls"
      contentText = "Press W, A, S, D to move, F to change window size,\nP to pause and ESC to exit."
  }.showAndWait()
  
  /*
   * Chooses an appropriate brightness for a texture 
   * according to player's distance from the wall
   */
  def getTextureWithBrightness(texNumber: Int, distance: Double) = {
    textureStripes(texNumber)((10 * distance).toInt)
  }
  /*
   * Paints the floor (the area below the horizon and walls)
   * and the ceiling (the area above the horizon and walls)
   */
  def paintFloorAndCeiling() = {
    gc.clearRect(0, 0, windowWidth, windowHeight)
    val ceilingColor = new Color("lightblue")
    val floorColor = new Color("gray")
    gc.setFill(ceilingColor)
    /*
     * Paint the ceiling. The height of the ceiling depends 
     * on the player's vertical heading. The higher the player
     * is heading, the larger the ceiling appears on the window.
     */
    gc.fillRect(0, 0, windowWidth, windowHeight / 2 + windowHeight * player.getHeadingY / player.fov)
    gc.setFill(floorColor)
    /*
     * Paint the floor. The y position and height of the floor
     * depends on the player's vertical heading. The lower the 
     * player is heading, the larger the floor appears.  
     */
    gc.fillRect(0, windowHeight / 2 + windowHeight * player.getHeadingY / player.fov, 
                windowWidth, windowHeight / 2 - windowHeight * player.getHeadingY / player.fov)
  }
  
  /*
   * This method clears the screen and paints the walls every frame 
   */
  def paint() = {
    /*
     * Paint the floor and ceiling before the walls
     */
    paintFloorAndCeiling()
    /*
     * The walls are filtered out if their distance to the player is 
     * greater than the rendering distance and then sorted according
     * to distance. The distance, in this case, is defined as the 
     * distance from the player to the vertex of a wall that is further
     * away from the player.
     */
    val walls = world.getWalls.filter(wall => (wall.v1 - player.getLocation).lengthSq < renderingDistance * renderingDistance)
                              .sortBy(wall => math.max((wall.v1 - player.getLocation).lengthSq, (wall.v2 - player.getLocation).lengthSq))
                              
    //Go through each vertical stripe of pixels in the screen
    for(x <- 0 until windowWidth) { 
      /*
       * The angle of each ray depends on the field of view 
       * and the width of the window.
       */
      val rayHeading = player.getHeadingX + (x - windowWidth / 2) * player.fov / windowWidth 
      /*
       * Create a ray from current location to the direction
       * of rayHeading to a distance equivalent to rendering distance.
       */
      val ray = new Line(player.getLocation, new Vec(player.getLocation.x + renderingDistance * math.sin(rayHeading), 
                                                     player.getLocation.y + renderingDistance * math.cos(rayHeading)))
      
      var wallDrawn = false
      var wallIndex = 0
      /*
       * Go through the possible intersections of walls starting from the wall closest
       * to the player until a wall is drawn for this ray or until all of the walls have 
       * been gone through without finding an intersection
       */
      while(!wallDrawn && wallIndex < walls.size) { 
        //Select the wall with the current index
        val wall = walls(wallIndex)
        wallIndex += 1
        val intersection = ray.lineIntersect(wall)
        intersection match {
          case Some(intersection) => 
            //The distance from the player to the intersection point
            val distance = math.sqrt((ray.v1 - intersection).lengthSq)
            /*
             * The rectangle height is divided by math.cos(heading - rayHeading)
             *  to get rid of the fisheye effect, that is the effect which makes
             *  objects in the center of the screen appear larger than near the 
             *  edges. The height is scaled up by a factor of 1.5 to make the
             *  walls look larger.  
             */
            val rectHeight = (1.5 * windowHeight / (distance * player.fov * math.cos(player.getHeadingX - rayHeading))).toInt
            /*
             * The y position of the rectangle on the window. Depends on the
             * height of the window, the player's vertical heading and fov.
             */
            val rectY = (windowHeight - rectHeight) / 2 + windowHeight * player.getHeadingY / player.fov
            //Get the appropriate texture according to the distance
            val texture = getTextureWithBrightness(wall.tex, distance)
            
            /*
             * The x location of the intersection point in the texture.
             * This is required to be able to draw an appropriate one pixel
             * wide stripe of the texture
             */
            val textureX = ((intersection.x % 1 + intersection.y % 1).abs * texture.getWidth()).toInt
            //Draw the one pixel wide stripe of the wall
            gc.drawImage(texture, textureX, 0, 1, texture.getHeight(), x, rectY, 1, rectHeight)
            //Set wallDrawn true to exit the while loop
            wallDrawn = true
          case None =>
        }
        
      }
    }
    
  }
  
  private var previousTime: Long = 0
  val timer = AnimationTimer(t => { 
    /*
     * The elapsed time can be 0.1 seconds maximum to avoid huge gaps in 
     * movement after a pause
     */
    val elapsedTime = math.min((t - previousTime) / 1000000000.0, 0.1) //The elapsed time in seconds
    previousTime = t
    /*
     * The player movement is handled according to which keys 
     * are pressed
     */
    (wPressed, sPressed, aPressed, dPressed) match {
      case (true, false, true, false) => player.moveFL(elapsedTime)
      case (true, false, false, true) => player.moveFR(elapsedTime)
      case (true, false, _, _) => player.moveForward(elapsedTime)
      case (false, true, true, false) => player.moveBL(elapsedTime)
      case (false, true, false, true) => player.moveBR(elapsedTime)
      case (false, true, _, _) => player.moveBack(elapsedTime)
      case (_, _, true, false) => player.moveLeft(elapsedTime)
      case (_, _, false, true) => player.moveRight(elapsedTime)
      case (_, _, _, _) => 
    }
    //Paint the walls on the window
    paint()
    })
  timer.start()
  
  
  /*
   * The robot allows the cursor to stay in the center of the screen
   * even though the mouse is moved. This way the mouse can be 
   * moved continuously in one direction without the movement 
   * suddenly stopping due to the cursor being out of the window
   */
  val robot = com.sun.glass.ui.Application.GetApplication.createRobot()
  
  canvas.setOnMouseMoved{e => {
    /*
     * If the game is paused, the cursor can be moved out
     *  of the window and nothing happens when moving it 
     *  on the window
     */
    if(!paused) {
      /*
       * The horizontal movement of the mouse.
       * Movement to the right is considered positive.
       */
      val dx = e.screenX - (this.getX() + this.getWidth() / 2)
      /*
       * The vertical movement of the mouse. Movement 
       * up is considered positive. 
       */
      val dy = (this.getY() + this.getHeight() / 2) - e.screenY 
      player.turnX(dx / 2000)
      player.turnY(dy / 2000)
      //The robot moves the mouse back to the center of the window
      robot.mouseMove((this.getX() + this.getWidth() / 2).toInt, (this.getY() + this.getHeight() / 2).toInt)
    }
  }}

  /*
   * These variables allow the movement to any direction 
   * and to two directions simultaneously.
   */
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
      //Changes the size of the window 
      case KeyCode.F => {
        if(!this.fullScreen.value) setFullScreen()
        else setSmallWindow()
      }
      //Pauses the program
      case KeyCode.P => if(paused) continue() else pause()
      //Closes the application
      case KeyCode.Escape => this.close() 
      case _ => 
    }
  }
  //Allows stopping the movement when a key is released
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