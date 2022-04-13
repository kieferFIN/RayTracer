package fi.kiefer.ray_tracer

import java.awt.image.BufferedImage
import scala.collection.parallel.immutable.ParVector

case class Camera(
                   orig:         Vec3,
                   width:        Int,
                   heigh:        Int,
                   upperLeft:    Vec3,
                   dy:           Vec3,
                   dx:           Vec3,
                   threads:      Int,
                   picWidth:     Int,
                   picHeight:    Int,
                   steps:        Int,
                   raysPerPixel: Int
                 ):
  def takePic(world: World):BufferedImage =
    val img = BufferedImage(width,heigh,BufferedImage.TYPE_INT_RGB)
    val indexes = ParVector.tabulate(width*heigh)(i => (i%width,i/width))
    indexes.map(i =>
      (i._1, i._2, world.shootRay(Ray.look_at(orig, upperLeft + dx * i._1  + dy * i._2),steps))
    ).foreach(d => img.setRGB(d._1,d._2,d._3.toPixel))

    img


class CameraBuilder(
                     var orig:                Vec3,
                     var size:                Tuple2[Int, Int],
                     var dir:                 Vec3,
                     var up:                  Vec3,
                     var horizontalAngle:     Int,
                     var superSamplingFactor: Int,
                     var raysPerPixel:        Int,
                     var steps:               Int,
                     var threads:             Int
                   ):
  def lookAt(at:Vec3) =
    dir = at - orig

  def build(): Camera = {
    val width = size._1*superSamplingFactor
    val height = size._2*superSamplingFactor
    val ratio = height.toDouble / width.toDouble
    val halfAngle = (horizontalAngle * 0.5).toRadians
    val dir = this.dir.normalize
    val right = dir.cross(up).normalize
    val down = dir.cross(right).normalize
    val w = math.tan(halfAngle) * 2.0
    val h = w * ratio
    val wVecor = right*w
    val hVecor = down*h
    val upperLeft = orig+dir-wVecor * 0.5 - hVecor * 0.5
    val dx = wVecor / width.toDouble
    val dy = hVecor / height.toDouble

    Camera(orig, width, height, upperLeft, dy, dx, threads, size._1, size._2, raysPerPixel, steps)
  }

object CameraBuilder:
  def apply(): CameraBuilder = new CameraBuilder(
    (0.0,0.0,0.0),
    (400,300),
    (0.0,0.0,-1.0),
    (0.0,1.0,0.0),
    65,
    2,
    5,
    3,
    4
  )
