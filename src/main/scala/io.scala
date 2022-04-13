package fi.kiefer.ray_tracer

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object io:
  def readObjFile(fileName: String): Vector[Triangle] =
    val vertices = ArrayBuffer[Vec3]()
    val normals = ArrayBuffer[Vec3]()
    val faces = ArrayBuffer[Triangle]()
    var currentColor = Color.grey(0.5)
    val materials = mutable.HashMap[String, Color]()
    val fileBuffer = Source.fromFile(fileName)
    for line <- fileBuffer.getLines do
      val tokens = line.split(' ')
      tokens(0) match
        case "v" => vertices.addOne((tokens(1).toDouble, tokens(2).toDouble, tokens(3).toDouble))
        case "vn" => normals.addOne((tokens(1).toDouble, tokens(2).toDouble, tokens(3).toDouble))
        case "f" =>
          val b = TriangleBuilder(currentColor)
          for i <- 1 until 4 do
            val indexes: Array[String] = tokens(i).split('/')
            b.add(vertices(indexes(0).toInt - 1), normals(indexes(2).toInt - 1))
          faces.addOne(b.build.get)
        case "usemtl" => currentColor = materials(tokens(1))
        case "mtllib" => materials ++= parseMtlFile(tokens(1))
        case _ => ()

    fileBuffer.close
    faces.toVector

  def parseMtlFile(fileName: String): mutable.HashMap[String, Color] =
    var materialName: Option[String] = None
    var material = Color.black
    val materials = mutable.HashMap[String, Color]()
    val fileBuffer = Source.fromFile(fileName)
    for line <- fileBuffer.getLines do
      val tokens = line.split(' ')
      tokens(0) match
        case "newmtl" =>
          materialName.foreach(materials(_) = material)
          materialName = Some(tokens(1))
        case "Kd" => material = Color(tokens(1).toDouble, tokens(2).toDouble, tokens(3).toDouble)
        case _ => ()

    materialName.foreach(materials(_) = material)

    fileBuffer.close
    materials




