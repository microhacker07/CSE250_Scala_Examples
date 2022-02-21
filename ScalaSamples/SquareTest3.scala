/** File "SquareTest3.scala" by KWR for CSE250, Spring 2022.
    For "Square Is-A Rectangle?" problem.
 */
class Rectangle(var length: Double, var width: Double) {
   def dims = s"$length x $width"
}

/*case*/ class Square(side: Double) extends Rectangle(side, side) {
   def side = width
}

object SquareTest3 extends App {
   var r = new Rectangle(2.0,3.0)
   r = new Square(3.0)   
   //r.length = 4.0    //caught by compiler since *data* is immutable
   println("The Square object has dimensions " + r.dims)
   val s = new Square(7.0)
   println(s"Does the Square object have a side? ${s.side}")
}

