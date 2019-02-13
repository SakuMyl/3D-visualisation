package src

class Wall(v1: Vec, v2: Vec) extends Line(v1, v2) {
//  val color = {
//    val r = scala.util.Random.nextInt(3)
//    r match {
//      case 0 => "red"
//      case 1 => "green"
//      case 2 => "blue"
//    }
//  }
}
class Line(val v1: Vec, val v2: Vec) {
  //Checks whether two lines share the same vertices, walls are always created from the text file characters
  //in a clockwise manner. Hence the first vertex of one wall should be the second vertex of another. 
  def equals(another: Line) = {
    (this.v1.equals(another.v1) && this.v2.equals(another.v2)) ||
    (this.v1.equals(another.v2) && this.v2.equals(another.v1))
  }
  /* The calculation of the intersection:
   * If two lines intersect, then there exist t and s 
   * so that this.v1 + s * u = wall.v1 + t * v, where
   * s and t are scalars, u = this.v2 - this.v1 and
   * v = wall.v2 - wall.v1. s and t can be solved from 
   * this equation by using cross products to eliminate
   * s (when solving for t) and t (when solving for s)
   * s * u x u = 0 and t * v x v = 0
   */
  def lineIntersect(wall: Line) = {
    val u = new Vec(this.v2.x - this.v1.x, this.v2.y - this.v1.y)
    val v = new Vec(wall.v2.x - wall.v1.x, wall.v2.y - wall.v1.y)
    
    val vxu = v.x * u.y - v.y * u.x //Magnitude of the cross product of v and u
    val w = new Vec(this.v1.x - wall.v1.x, this.v1.y - wall.v1.y)
    val t = (w.x * u.y - w.y * u.x) / vxu
    val s = (w.x * v.y - w.y * v.x) / vxu      
    if(t >= 0 && t <= 1 && s >= 0 && s <= 1) {
      Some(new Vec(wall.v1.x + t * v.x, wall.v1.y + t * v.y))
    }
    else {
      None
    }
  }
}