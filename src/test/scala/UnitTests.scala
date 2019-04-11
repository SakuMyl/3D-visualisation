package src

import main.scala._

import org.scalatest.FlatSpec

class UnitTests extends FlatSpec {
   
  "lineIntersect" should "return the correct location when the intersection point is between the both lines' vertices" in {
    val line = new Line(new Vec(0, 0), new Vec(10, 0))
    val another = new Line(new Vec(5, -5), new Vec(5, 5))
    val intersect1 = line.lineIntersect(another)
    
    assert(intersect1.isDefined)
    assert(intersect1.get.x == 5.0)
    assert(intersect1.get.y == 0.0)
  }
  "lineIntersect" should "return the correct location when the intersection point is a vertex of one line" in {
    val line = new Line(new Vec(0, 0), new Vec(10, 0))
    val another = new Line(new Vec(0, -5), new Vec(0, 5))
    val intersect = line.lineIntersect(another)
    
    assert(intersect.isDefined)
    assert(intersect.get.x == 0.0)
    assert(intersect.get.y == 0.0)
  }
  "lineIntersect" should "return the correct location when the intersection point is a shared vertex of two lines" in {
    val line = new Line(new Vec(0, 0), new Vec(10, 0))
    val another = new Line(new Vec(0, 0), new Vec(0, 10))
    //For the case where the two lines are parallel
    val third = new Line(new Vec(0,0), new Vec(-10, 0))
    val intersect = line.lineIntersect(another)
    val intersect2 = line.lineIntersect(third)
    
    assert(intersect.isDefined)
    assert(intersect2.isDefined)
    assert(intersect.get.x == 0.0)
    assert(intersect.get.y == 0.0)
  }
  "lineIntersect" should "not be defined when the lines don't intersect" in {
    val line = new Line(new Vec(0, 0), new Vec(10, 0))
    val another = new Line(new Vec(-5, -5), new Vec(-5, 5))
    
    //For cases where the lines are parallel but disjoint
    val parallel = new Line(new Vec(-15, 0), new Vec(-5, 0))
    val parallel2 = new Line(new Vec(0, 5), new Vec(10, 5))
    
    val intersect = line.lineIntersect(another)
    val parallelIntersect = line.lineIntersect(parallel)
    val parallelIntersect2 = line.lineIntersect(parallel2)
    
    assert(intersect.isEmpty)
    assert(parallelIntersect.isEmpty)
    assert(parallelIntersect2.isEmpty)
  }
  "lineIntersect" should "be defined when the lines overlap" in {
    val line = new Line(new Vec(0, 0), new Vec(10, 0))
    val line2 = new Line(new Vec(10, 0), new Vec(0, 0))
    val line3 = new Line(new Vec(-5, 0), new Vec(5, 0))
    val line4 = new Line(new Vec(-5, 0), new Vec(15, 0))
    val intersect = line.lineIntersect(line3)
    val intersect2 = line.lineIntersect(line4)
    val intersect3 = line2.lineIntersect(line3)
    val intersect4 = line2.lineIntersect(line4)
    
    assert(intersect.isDefined)
    assert(intersect2.isDefined)
    assert(intersect3.isDefined)
    assert(intersect4.isDefined)
  }
}