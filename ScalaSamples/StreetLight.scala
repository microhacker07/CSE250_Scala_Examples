/** File StreetLight.scala, by KWR for CSE250, Spring 2022
    "Midway" implementation of the Lacher-Lewis text's example on page 136,
    without using the "syntactic sugar" provided by the library's Enumeration clas.
 */

sealed trait StreetLightColor

case object Red extends StreetLightColor     //case object since there is only one "Red"
case object Yellow extends StreetLightColor
case object Green extends StreetLightColor

class StreetLight(private var _color: StreetLightColor) {
   def color = _color

   //no need to import when done this way

   def cycle: Unit = _color match {   //this is a mutator procedure
      case Green => _color = Yellow   //here Yellow is a zero-parameter constructor call
      case Yellow => _color = Red     //that constructs an object of type StreetLightColor
      case Red => _color = Green
   }
}

object StreetLight extends App {
   val light = new StreetLight(Red)
   for (i <- 0 until 4) {   //4 is exclusive
      light.cycle
   }
   println("After 4 cycles starting from Red, the light ended up " + light.color)
}
   
