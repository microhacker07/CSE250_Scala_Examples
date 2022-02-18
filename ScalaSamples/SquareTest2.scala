/** File "SquareTest2.scala" by KWR for CSE250, Spring 2022.
    For "Square Is-A Rectangle?" problem.
 */

class Rectangle(val length: Double, val width: Double) {
   def dims = s"$length x $width"
}
class Square(val side: Double) extends Rectangle(side, side)

object SquareTest2 extends App {
   var r: Rectangle = new Square(3.0)
   //r.length = 4.0    //caught by compiler since *data* is immutable
   println("The Square object has dimensions " + r.dims)
}

