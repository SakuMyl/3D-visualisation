package main.scala

class Vec(val x: Double, val y: Double) {
  
  /*
   * The length is calculated as the square of the 
   * actual length by default to increase performance
   */
  def lengthSq = this.x * this.x + this.y * this.y
  
  def -(another: Vec) = new Vec(this.x - another.x, this.y - another.y)
  
  def dotProduct(another: Vec) = this.x * another.x + this.y * another.y
  
  /*
   * The 2D cross product is a special case of vector
   * cross product. For two vectors u, v so that 
   * u = a*i + b*j + c*k and v = d*i + e*j + f*k
   * where c,f = 0 since the vectors are 2D.
   * Now the cross product u x v becomes 
   * (a * d)i x i + (a * e)i x j + (b * d)j x i + (b * e)j x j =
   * (a * e)k - (b * d)k
   * In this case, we are left with a vector with a k factor.
   * Since this is a 2D cross product, we don't care about the 
   * k direction, only the magnitude of the cross product. 
   * Ignoring the direction, the cross product becomes a * e - b * d
   */
  def crossProduct(another: Vec) = this.x * another.y - this.y * another.x
  
  /*
   * Allows comparison of the locations of two Vec objects
   */
  def equals(another: Vec) = this.x == another.x && this.y == another.y
  
  
 
  override def toString = "Vector at (" + this.x + ", " + this.y + ")" 
}

