package fi.kiefer.ray_tracer

import fi.kiefer.ray_tracer.{Color, Vec3}
import fi.kiefer.ray_tracer.vec2array
import breeze.linalg.{DenseMatrix, DenseVector}

import scala.collection.mutable.ListBuffer

case class Triangle(color:     Color,
                    verticies: (Vec3, Vec3, Vec3),
                    normals:   (Vec3, Vec3, Vec3),
                    ab:        Vec3,
                    ac:        Vec3
                   ):
  def hit(r: Ray): Option[Hit] =
    try
      val m = new DenseMatrix(3, ab.toArray ++ ac.toArray ++ r.dir.toArray, 0)
      val b = new DenseVector(verticies(0) - r.orig)
      val x = m \ b
      val beta = x(0)
      val gamma = x(1)
      val t = x(2)
      val alpha = 1.0 - beta - gamma
      if beta < 0.0 || gamma < 0.0 || alpha < 0.0 then
        return None
      val n = normals._1 * alpha + normals._2 * beta + normals._3 * gamma
      if t > 0.0 && n.dot(r.dir) < 0.0 then
        Some(Hit(t, n, color))
      else
        None
    catch
      case e:breeze.linalg.MatrixSingularException => None

class TriangleBuilder(val color: Color):
  private val verticies = ListBuffer[Vec3]()
  private val normals = ListBuffer[Vec3]()

  def add(v: Vec3, n: Vec3) =
    verticies += v
    normals += n

  def build: Option[Triangle] =
    if verticies.size != 3 && normals.size != 3 then
      None
    else
      val v = verticies match
        case ListBuffer(x, y, z) => (x, y, z)
      val n = normals match
        case ListBuffer(x, y, z) => (x, y, z)
      Some(Triangle(color, v, n, v._1 - v._2, v._1 - v._3))

