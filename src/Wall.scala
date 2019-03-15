package src

import scalafx.scene.canvas.Canvas
import scalafx.scene.image.Image

/*
 * A wall is practically a line with the addition
 * of a number of the texture it should be drawn with.
 */
class Wall(v1: Vec, v2: Vec, val tex: Int) extends Line(v1, v2) 

class Line(val v1: Vec, val v2: Vec) {
  //Checks whether two lines share the same vertices.
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
  def lineIntersect(wall: Wall) = {
    val u = this.v2 - this.v1
    val v = wall.v2 - wall.v1
    
    val vxu = v.crossProduct(u) //Magnitude of the cross product of v and u
    val w = this.v1 - wall.v1
    val r = w.crossProduct(u)
    val q = w.crossProduct(v)
    
    if(vxu == 0 && r == 0) {
      val t0 = w.dotProduct(u) / (u.dotProduct(u))
      if(u.dotProduct(u) == 0) println(t0)
      val t1 = t0 + (v.dotProduct(u) / (u.dotProduct(u)))
      
      if((t0 <= 1 && t1 >= 0) || (t0 >= 0 && t1 <= 1)) {
        Some((wall.v1, wall.tex))
      }
      else {
        None
      }
    } else {
      
      val t = r / vxu
      val s = q / vxu   
      
      val epsilon = 0.000001
      if(t > -epsilon && t < 1 + epsilon && s > -epsilon && s < 1 + epsilon) {
        Some((new Vec(wall.v1.x + t * v.x, wall.v1.y + t * v.y), wall.tex))
      }
      else {
        None
      }
    }
  }
  
}