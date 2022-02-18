/**File RealOrComplex.scala, by KWR for CSE250, Spring 2022
   Illustrates setters and case classes and value typesaout of Ch. 3
   Resembles the text's Vector2D example but does different stuff
 */

import io.StdIn._
import io.Source
import scala.math._         //for cos(...) and sin(...)
import java.io.File
import java.io.FileWriter   //makes it easy to append
import java.io.PrintWriter  //makes "print" and "println" available

/** Complex numbers as re^{i.theta}
    Argument variables need type declarations
 */
class ComplexPolar(private var _r: Double, private var _theta: Double) {   
    def r = _r
    def theta = _theta
    def r_=(newMag: Double): Unit = if (newMag < 0.0) {_r = 0.0} else {_r = newMag}
    def theta_=(newTheta: Double): Unit = _theta = newTheta
    //makes other code in the class normalize 0 <= theta < 2pi if it wants to.
    def x = r*cos(theta)
    def y = r*sin(theta)
}


sealed trait Number   //can have empty body; "sealed" means only cases must be in this file.

/** Complex numbers as re^{i.theta}
    Args are automatically values.  Still need type declarations.
 */
case class ComplexPolarVal(r: Double, theta: Double) extends Number {
    def x = r*cos(theta)
    def y = r*sin(theta)
}

case class RealVal(x: Double) extends Number

object RealOrComplex2 extends App {
   val z = ComplexPolarVal(1.0,Pi)
   println(s"The polar number $z has real part ${z.x} and complex part ${z.y}")

   
   //Return imaginary part of number
   def imagPart(w: Number) = w match {
      case RealVal(x) => 0.0
      case ComplexPolarVal(r,theta) => ComplexPolarVal(r,theta).y
      //case _ => 0.0                     //compiler knows default not needed
   }                                      //since Number is a sealed trait


   val u = RealVal(3.0)
   val v = ComplexPolarVal(1.0,Pi/2)
   println(s"The imaginary part of u is ${imagPart(u)} and of v is ${imagPart(v)}")
}
   
