package fi.kiefer.ray_tracer

case class Color(r: Double, g: Double, b: Double):

  def *(that: Color): Color = Color(r * that.r, g * that.g, b * that.b)
  def *(that: Double): Color = Color(r* that, g* that, b* that)
  def +(that: Color): Color = Color(r + that.r, g + that.g, b + that.b)

  def toPixel = ((r.max(0.0).min(1.0)*255).toInt << 16) + ((g.max(0.0).min(1.0)*255).toInt << 8) + ((b.max(0.0).min(1.0)*255).toInt)
  def toBW =
    val v = (((r+g+b)/3.0).max(0.0).min(1.0)*255).toInt
    (v<<16)+(v << 8) + v


object Color:
  def grey(c: Double): Color = Color(c, c, c)
  def black: Color = grey(0)
  def white: Color = grey(1)

given tuple2color: Conversion[(Double, Double, Double), Color] with
  def apply(x: (Double, Double, Double)): Color = Color(x._1, x._2, x._3)
