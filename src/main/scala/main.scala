package fi.kiefer.ray_tracer

import javax.imageio.ImageIO
import java.io.File
import scala.collection.mutable.ListBuffer



@main def ray_tracer(fileName: String) = {
  val t = Timer()
  t.stamp
  val triangles = io.readObjFile(fileName)
  t.stamp
  val light = Light((-0.2,0.9,-0.2),(0.4,0.0,0.0),(0.0,0.0,0.4),(3.0,2.9,2.0))
  val world = World(triangles, light)
  t.stamp
  var cameraBuilder = CameraBuilder()
  cameraBuilder.orig = (0.0,0.0,-3.0)
  cameraBuilder.lookAt((0.0,0.1,0.0))
  cameraBuilder.superSamplingFactor=1
  cameraBuilder.raysPerPixel=1
  cameraBuilder.steps=1
  cameraBuilder.size=(400,600)
  val camera = cameraBuilder.build()
  t.stamp
  val pic = camera.takePic(world)
  t.stamp
  ImageIO.write(pic,"png", File("./test.png"))
  t.stamp
  t.print()
}

class Timer:
  val moments:ListBuffer[Long] = ListBuffer()

  def stamp = moments.addOne(System.nanoTime())

  def print() =
    moments.tails.map(_.take(2)).filter(_.length==2).foreach(d => println((d(1)-d(0))/1e6))
