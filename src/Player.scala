package src

class Player(location: Vec, heading: Double, val world: World) {
  
  private var currentHeading = heading
  
  private var currentLocation = location
  
  val fovInDegrees = 80 //Field of view in degrees
  val fov = math.toRadians(fovInDegrees) //Converted to radians
  val sensitivity = 5 //Determines how much moving the mouse turns the camera. 1 is very low, 10 is very high
  
  def turn(dx: Double) = {
    currentHeading += dx * sensitivity
  }
  
  
  def moveForward(elapsedTime: Double) = move(math.sin(currentHeading), math.cos(currentHeading), elapsedTime)
  def moveRight(elapsedTime: Double)   = move(math.cos(currentHeading), -math.sin(currentHeading), elapsedTime)
  def moveLeft(elapsedTime: Double)    = move(-math.cos(currentHeading), math.sin(currentHeading), elapsedTime)
  def moveBack(elapsedTime: Double)    = move(-math.sin(currentHeading), -math.cos(currentHeading), elapsedTime)
  //Move forward and right
  def moveFR(elapsedTime: Double)      = move(math.sin(currentHeading + math.Pi / 4), math.cos(currentHeading + math.Pi / 4), elapsedTime)
  //Move forward and left
  def moveFL(elapsedTime: Double)      = move(math.sin(currentHeading - math.Pi / 4), math.cos(currentHeading - math.Pi / 4), elapsedTime)
  //Move back and right
  def moveBR(elapsedTime: Double)      = move(-math.sin(currentHeading - math.Pi / 4), -math.cos(currentHeading - math.Pi / 4), elapsedTime)
  //Move back and left
  def moveBL(elapsedTime: Double)      = move(-math.sin(currentHeading + math.Pi / 4), -math.cos(currentHeading + math.Pi / 4), elapsedTime)
  
  def move(xChange: Double, yChange: Double, elapsedTime: Double) = {
    //The multiplier 3 is here to make movement faster 
    val coEfficient = 3 * elapsedTime
    /*
     * For all walls near the player, check whether the player is trying to move through them before allowing the change of location.
     * The change of location is done for both x- and y-components separately to allow the player to "slide" on walls.
     */
    val wallsNearby = world.getWalls.filter(wall => (wall.v1 - currentLocation).length < 2)
    val newXLocation = new Vec(currentLocation.x + coEfficient * xChange, currentLocation.y)
    if(wallsNearby.forall (wall => new Line(currentLocation, newXLocation).lineIntersect(wall).isEmpty )) currentLocation = newXLocation
    val newYLocation = new Vec(currentLocation.x, currentLocation.y + coEfficient * yChange)
    if(wallsNearby.forall (wall => new Line(currentLocation, newYLocation).lineIntersect(wall).isEmpty )) currentLocation = newYLocation
  }
  
  def wallWithinFov(wall: Wall) = {
    def pointWithinFov(point: Vec) = {
      //Unit vector of the player's heading
      val headingUnitized = new Vec(math.sin(currentHeading), math.cos(currentHeading))
      val diff = point - currentLocation
      val length = diff.length
      //Take the dot product of the heading and diff
      val dotProduct = headingUnitized.dotProduct(new Vec(diff.x / length, diff.y / length))
      dotProduct > math.cos(fov / 2) && length < Demo.renderingDistance
    }
    pointWithinFov(wall.v1) || pointWithinFov(wall.v2) || 
    new Line(currentLocation, new Vec(currentLocation.x + 100 * math.sin(currentHeading),
    currentLocation.y + 100 * math.cos(currentHeading))).lineIntersect(wall).isDefined
  }
  def getHeading = this.currentHeading
  def getLocation = this.currentLocation
}