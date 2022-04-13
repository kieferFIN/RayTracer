package fi.kiefer.ray_tracer

import scala.math.sqrt

case class Vec3(x: Double, y: Double, z: Double):
  def +(that: Vec3): Vec3 = Vec3(x + that.x, y + that.y, z + that.z)
  def -(that: Vec3): Vec3 = Vec3(x - that.x, y - that.y, z - that.z)
  def *(that: Double): Vec3 = Vec3(x * that, y * that, z * that)
  def /(that: Double): Vec3 = Vec3(x / that, y / that, z / that)

  def dot(that: Vec3): Double = x * that.x + y * that.y + z * that.z
  def cross(that: Vec3): Vec3 = Vec3(y * that.z - that.y, z * that.x - x * that.z, x * that.y - y * that.x)

  def norm_squared: Double = this.dot(this)
  def norm: Double = sqrt(this.norm_squared)
  def normalize: Vec3 = this / norm

  def toArray: Array[Double] = Array(x, y, z)

given vec2array: Conversion[Vec3, Array[Double]] = _.toArray

given tuple2vec: Conversion[(Double, Double, Double), Vec3] with
  def apply(x: (Double, Double, Double)): Vec3 = Vec3(x._1, x._2, x._3)