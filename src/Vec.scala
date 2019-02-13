package src

import scalafx.scene.paint.Color

class Vec(val x: Double, val y: Double) {
  
  val length = math.sqrt(this.x * this.x + this.y * this.y) 
  
  def -(another: Vec) = new Vec(this.x - another.x, this.y - another.y)
  
  def dotProduct(another: Vec) = this.x * another.x + this.y * another.y
  
  def crossProduct(another: Vec) = this.x * another.y - this.y * another.x
  
  def equals(another: Vec) = this.x == another.x && this.y == another.y
  
 
  //Determines the angle between two vectors, TODO
  def unitize() = new Vec(x / length, y / length)
  
  override def toString = "Vector at (" + this.x + ", " + this.y + ")" 
}


//This class is used to draw rectangles on the screen according to where rays intersect with walls
class Rectangle(val screenPosition: Double, val distance: Double, val height: Int, val color: Color)

