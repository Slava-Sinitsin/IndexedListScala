class Point2D(private var x: Double, private var y: Double) extends Comparable[Point2D] {
  def setX(newX: Double): Unit = {
    x = newX
  }

  def setY(newY: Double): Unit = {
    y = newY
  }

  def getX: Double = x

  def getY: Double = y

  override def toString: String = s"($x, $y)"

  override def compareTo(other: Point2D): Int = {
    val distance1 = Math.sqrt(x * x + y * y)
    val distance2 = Math.sqrt(other.x * other.x + other.y * other.y)
    java.lang.Double.compare(distance1, distance2)
  }
}
