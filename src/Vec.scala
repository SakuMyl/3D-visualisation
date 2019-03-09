package src

import scalafx.scene.paint.Color
import scalafx.scene.image.Image

class Vec(val x: Double, val y: Double) {
  
  def lengthSq = this.x * this.x + this.y * this.y
  
  def -(another: Vec) = new Vec(this.x - another.x, this.y - another.y)
  
  def dotProduct(another: Vec) = this.x * another.x + this.y * another.y
  
  def crossProduct(another: Vec) = this.x * another.y - this.y * another.x
  
  def equals(another: Vec) = this.x == another.x && this.y == another.y
  
  def unitize() = {
    val length = math.sqrt(this.lengthSq)
    new Vec(this.x / length, this.y / length)
  }
  
 
  override def toString = "Vector at (" + this.x + ", " + this.y + ")" 
}

//This class is used to draw rectangles on the screen according to where rays intersect with walls
class Rectangle(val screenPosition: Double, val distance: Double, val height: Int, val texture: Image)

