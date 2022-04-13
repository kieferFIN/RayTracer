package fi.kiefer.ray_tracer

import fi.kiefer.ray_tracer.Vec3

import scala.collection.immutable.Vector

case class Light(o: Vec3, a: Vec3, b: Vec3, n: Vec3, I: Color, A: Double):
  def samplePoints: Vector[Vec3] =
    val r = scala.util.Random
    Vector(
      o + a * r.nextDouble * .5 + b * r.nextDouble * .5,
      o + a * (r.nextDouble * .5 + .5) + b * r.nextDouble * .5,
      o + a * (r.nextDouble * .5 + .5) + b * (r.nextDouble * .5 + .5),
      o + a * r.nextDouble * .5 + b * (r.nextDouble * .5 + .5)
    )

object Light:
  def apply(o: Vec3, a: Vec3, b: Vec3, I: Color): Light =
    val cross = a.cross(b)
    Light(o, a, b, cross.normalize, I, cross.norm)
    