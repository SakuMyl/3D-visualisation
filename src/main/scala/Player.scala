package main.scala

class Player(location: Vec, heading: Double, val world: World) {
  
  //The player's current heading in radians
  private var currentHeading = heading
  
  //The currentLocation of the player as a Vec object
  private var currentLocation = location
  
  val fovInDegrees = 80 //Field of view in degrees
  val fov = math.toRadians(fovInDegrees) //Converted to radians
  val sensitivity = 5 //Determines how much moving the mouse turns the camera. 1 is very low, 10 is very high
  val movingSpeed = 3 //The moving speed of the player
  
  /*
   * Turns the camera by changing the player's heading.
   * dx is the amount how much the camera should be turned.
   * Sensitivity also affects the turning speed.
   */
  def turn(dx: Double) = {
    currentHeading += dx * sensitivity
  }
  
  //Moves the player forward, i.e. to the direction of the player's heading.
  def moveForward(elapsedTime: Double) = move(math.sin(currentHeading), math.cos(currentHeading), elapsedTime)
  //Moves the player 90 degrees to the right from the player's heading
  def moveRight(elapsedTime: Double)   = move(math.cos(currentHeading), -math.sin(currentHeading), elapsedTime)
  //Moves the player 90 degrees to the left from the player's heading
  def moveLeft(elapsedTime: Double)    = move(-math.cos(currentHeading), math.sin(currentHeading), elapsedTime)
  //Moves the player to the opposite direction of the player's heading
  def moveBack(elapsedTime: Double)    = move(-math.sin(currentHeading), -math.cos(currentHeading), elapsedTime)
  //Moves the player 45 degrees to the right from the player's heading
  def moveFR(elapsedTime: Double)      = move(math.sin(currentHeading + math.Pi / 4), math.cos(currentHeading + math.Pi / 4), elapsedTime)
  //Moves the player 45 degrees to the left from the player's heading
  def moveFL(elapsedTime: Double)      = move(math.sin(currentHeading - math.Pi / 4), math.cos(currentHeading - math.Pi / 4), elapsedTime)
  ///Moves the player 135 degrees to the right from the player's heading
  def moveBR(elapsedTime: Double)      = move(-math.sin(currentHeading - math.Pi / 4), -math.cos(currentHeading - math.Pi / 4), elapsedTime)
  //Moves the player 135 degrees to the left from the player's heading
  def moveBL(elapsedTime: Double)      = move(-math.sin(currentHeading + math.Pi / 4), -math.cos(currentHeading + math.Pi / 4), elapsedTime)
  
  /*
   * Moves the player, i.e. changes the player's coordinates by some
   * x and y factors multiplied by the time elapsed. Doesn't allow moving
   * through walls.
   */
  def move(xChange: Double, yChange: Double, elapsedTime: Double) = {
    /*
     * How much the player moves depends on the time elapsed
     * and the moving speed.
     */
    val coEfficient = movingSpeed * elapsedTime
    /*
     * For all walls near the player, check whether the player is trying to move through them before allowing the change of location.
     * The change of location is done for both x- and y-components separately to allow the player to "slide" on walls.
     */
    val wallsNearby = world.getWalls.filter(wall => (wall.v1 - currentLocation).lengthSq < 2)
    val newXLocation = new Vec(currentLocation.x + coEfficient * xChange, currentLocation.y)
    if(wallsNearby.forall (wall => new Line(currentLocation, newXLocation).lineIntersect(wall).isEmpty )) currentLocation = newXLocation
    val newYLocation = new Vec(currentLocation.x, currentLocation.y + coEfficient * yChange)
    if(wallsNearby.forall (wall => new Line(currentLocation, newYLocation).lineIntersect(wall).isEmpty )) currentLocation = newYLocation
  }
  
  def getHeading = this.currentHeading
  def getLocation = this.currentLocation
}