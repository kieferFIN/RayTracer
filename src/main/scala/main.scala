package fi.kiefer.ray_tracer

import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable.ListBuffer


@main def ray_tracer(fileName: String): Unit = {
  val t = Timer()
  t.stamp()
  val triangles = io.readObjFile(fileName)
  t.stamp()
  val light = Light((-0.2, 0.9, -0.2), (0.4, 0.0, 0.0), (0.0, 0.0, 0.4), (3.0, 2.9, 2.0))
  val world = World(triangles, light)
  t.stamp()
  var cameraBuilder = CameraBuilder()
  cameraBuilder.orig = (0.0, 0.0, -3.0)
  cameraBuilder.lookAt((0.0, 0.1, 0.0))
  cameraBuilder.superSamplingFactor = 1
  cameraBuilder.raysPerPixel = 1
  cameraBuilder.steps = 2
  cameraBuilder.size = (200, 300)
  val camera = cameraBuilder.build()
  t.stamp()
  val pic = camera.takePic(world)
  t.stamp()
  ImageIO.write(pic, "png", File("./test.png"))
  t.stamp()
  t.print()
}

class Timer:
  val moments: ListBuffer[Long] = ListBuffer()

  def stamp(): Unit = moments.addOne(System.nanoTime())

  def print(): Unit =
    moments.tails.map(_.take(2)).filter(_.length == 2).foreach(d => println((d(1) - d.head) / 1e6))
