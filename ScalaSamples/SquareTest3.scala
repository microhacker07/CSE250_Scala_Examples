/** File "SquareTest3.scala" by KWR for CSE250, Spring 2022.
    For "Square Is-A Rectangle?" problem.
 */
case class Rectangle(length: Double, width: Double) {
   def dims = s"$length x $width"
}

/*case*/ class Square(val side: Double) extends Rectangle(side, side)

object SquareTest3 extends App {
   var r = Rectangle(2.0,3.0)
   r = new Square(3.0)   
   //r.length = 4.0    //caught by compiler since *data* is immutable
   println("The Square object has dimensions " + r.dims)
}

