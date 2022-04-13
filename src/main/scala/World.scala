package fi.kiefer.ray_tracer

import scala.collection.mutable.ListBuffer

case class World(triangles: Vector[Triangle], light: Light):
  def shootRay(ray: Ray, steps: Int): Color =
    val eps = 0.01
    var c = Color.white
    var r = ray
    var color = Color.black
    var stop = false
    for _ <- 0 until steps if !stop do
      triangles.flatMap(_.hit(r)).minByOption(_.t) match
        case None => stop = true
        case Some(hit) =>
          c = c * hit.c
          val p = r.orig + r.dir * (hit.t - eps)
          //TODO: get dir from hemisphere
          r = Ray(p, hit.n)
          color += c * radianceFromLight(p, hit.n)

    color



  def radianceFromLight(p: Vec3, n: Vec3): Color =
    val est = light.samplePoints.map(s => {
      val dir = p - s
      val r = Ray(s, dir)
      if !isSomethingBlocking(r) then
        val d = dir.normalize
        light.n.dot(d).abs * -n.dot(d) / d.norm_squared
      else
        0.0
    }).sum
    light.I * (est * light.A / math.Pi).max(0.0)

  def isSomethingBlocking(r: Ray): Boolean =
    triangles.flatMap(_.hit(r)).exists(_.t < 1.0)


