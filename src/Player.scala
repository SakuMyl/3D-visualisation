package src

class Player(location: Vec, heading: Double, val world: World) {
  
  private var currentHeading = heading
  
  private var currentLocation = location
  
  val fovInDegrees = 80 //Field of view in degrees
  val fov = math.toRadians(fovInDegrees) //Converted to radians
  val sensitivity = 5 //Determines how much moving the mouse turns the camera. 1 is very low, 10 is very high
  
  def turn(dx: Double) = {
    currentHeading += dx * sensitivity * fov
//    if(currentHeading < 0) currentHeading += 2 * math.Pi
//    else if(currentHeading > 2 * math.Pi) currentHeading -= 2 * math.Pi
  }
  
  def moveForward(elapsedTime: Double) = move(math.sin(currentHeading), math.cos(currentHeading), elapsedTime)
  def moveRight(elapsedTime: Double)   = move(math.cos(currentHeading), -math.sin(currentHeading), elapsedTime)
  def moveLeft(elapsedTime: Double)    = move(-math.cos(currentHeading), math.sin(currentHeading), elapsedTime)
  def moveBack(elapsedTime: Double)    = move(-math.sin(currentHeading), -math.cos(currentHeading), elapsedTime)
  def moveFR(elapsedTime: Double)      = move(math.sin(currentHeading + math.Pi / 4), math.cos(currentHeading + math.Pi / 4), elapsedTime)
  def moveFL(elapsedTime: Double)      = move(math.sin(currentHeading - math.Pi / 4), math.cos(currentHeading - math.Pi / 4), elapsedTime)
  def moveBR(elapsedTime: Double)      = move(-math.sin(currentHeading - math.Pi / 4), -math.cos(currentHeading - math.Pi / 4), elapsedTime)
  def moveBL(elapsedTime: Double)      = move(-math.sin(currentHeading + math.Pi / 4), -math.cos(currentHeading + math.Pi / 4), elapsedTime)
  
  def move(xChange: Double, yChange: Double, elapsedTime: Double) = {
    //The multiplier 3 is here to make movement faster 
    val coEfficient = 3 * elapsedTime
    //For all walls near the player, check whether the player is trying to move through them before allowing the change of location
    val wallsNearby = world.getWalls.filter(wall => (wall.v1 - currentLocation).length < 2)
    val newXLocation = new Vec(currentLocation.x + coEfficient * xChange, currentLocation.y)
    if(wallsNearby.forall (wall => new Line(currentLocation, newXLocation).lineIntersect(wall).isEmpty )) currentLocation = newXLocation
    val newYLocation = new Vec(currentLocation.x, currentLocation.y + coEfficient * yChange)
    if(wallsNearby.forall (wall => new Line(currentLocation, newYLocation).lineIntersect(wall).isEmpty )) currentLocation = newYLocation
  }
  
  def wallWithinFov(wall: Line) = {
    def pointWithinFov(point: Vec) = {
      //Unit vector of the player's heading
      val headingUnitized = new Vec(math.sin(currentHeading), math.cos(currentHeading))
      val diff = (point - currentLocation).unitize()
      //Take the dot product of the heading and diff
      val dotProduct = headingUnitized.dotProduct(diff)
      dotProduct > math.cos(fov / 2)
    }
    pointWithinFov(wall.v1) || pointWithinFov(wall.v2) || 
    wall.lineIntersect(new Line(currentLocation, new Vec(currentLocation.x + 100 * math.sin(currentHeading),
    currentLocation.y + 100 * math.cos(currentHeading)))).isDefined
  }
  def getHeading = this.currentHeading
  def getLocation = this.currentLocation
}