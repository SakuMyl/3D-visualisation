package main.scala

import scalafx.scene.canvas.Canvas
import scalafx.scene.image.Image

/*
 * A wall is practically a line with the addition
 * of a number code of the texture it should be drawn with.
 */
class Wall(v1: Vec, v2: Vec, val tex: Int) extends Line(v1, v2) 

class Line(val v1: Vec, val v2: Vec) {
  /*
   * Allows comparison of two Line objects by checking
   * whether the two lines share the same vertices.
   */
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
    val u = this.v2 - this.v1  
    val v = wall.v2 - wall.v1  
    
    val uxv = u.crossProduct(v) 
    val w = wall.v1 - this.v1  
    val r = w.crossProduct(u)  
    val q = w.crossProduct(v)
    
    /*
     * uxv == 0, when the two line segments are parallel.
     * r == 0, when the first vertex of this line segment lies on the line
     * extending from the vertices of the second line segment. 
     * These two factors combined we have a pair of parallel line segments,
     * both of which lie on the same line extending from the two line 
     * segments' vertices.
     */
    if(uxv == 0 && r == 0) {
      /*
       * If t0 is between 0 and 1, then wall.v1 is between this.v1
       * and this.v2
       */
      val t0 = w.dotProduct(u) / (u.dotProduct(u))
      /*
       * If t1 is between 0 and 1, then wall.v2 is between this.v2
       * and this.v2
       */
      val t1 = ((wall.v2 - this.v1).dotProduct(u) / (u.dotProduct(u)))
      
      /*
       * Now check whether either of the vertices of 'wall' is between
       * the vertices of 'this'
       */
      if((t0 <= 1 && t1 >= 0) || (t0 >= 0 && t1 <= 1)) {
        Some(wall.v1)
      }
      else {
        None
      }
    } else {
      val t = q / uxv
      val s = r / uxv  
      
      val epsilon = 0.000001
      if(t > -epsilon && t < 1 + epsilon && s > -epsilon && s < 1 + epsilon) {
        Some(new Vec(wall.v1.x + s * v.x, wall.v1.y + s * v.y))
      }
      else {
        None
      }
    }
  }
  
}