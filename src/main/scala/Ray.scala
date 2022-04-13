package fi.kiefer.ray_tracer

case class Ray(orig: Vec3, dir:Vec3)

object Ray:
  def look_at(orig: Vec3, dest:Vec3):Ray = Ray(orig, dest-orig)
