name := "RayTracer"

version := "0.1"

scalaVersion := "3.0.2"

idePackagePrefix := Some("fi.kiefer.ray_tracer")
libraryDependencies += "org.scalanlp" %% "breeze" % "2.0-RC3"
libraryDependencies +=
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"