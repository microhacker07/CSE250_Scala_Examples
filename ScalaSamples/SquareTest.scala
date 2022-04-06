class Rectangle(var length: Double, var width: Double) {
   def dims = s"$length x $width"
}
class Square(var side: Double) extends Rectangle(side, side)

object SquareTest extends App {
   val r: Rectangle = new Square(3.0)
   r.length = 4.0    //note again: "fake const"
   println("The Square object has dimensions " + r.dims)
}
