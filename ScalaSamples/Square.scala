/** File "Square.scala" by KWR for CSE250, Spring 2022.
    For "Square Is-A Rectangle?" problem.
 */
case class Rectangle(length: Double, width: Double) {
   def dims = s"$length x $width"
}

//case
class Square(var side: Double) extends Rectangle(side, side)

object SquareTest extends App {
   var r: Rectangle = new Square(3.0)
   //r.length = 4.0
   println("The Square object has dimensions " + r.dims)
}
