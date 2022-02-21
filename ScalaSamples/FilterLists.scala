/** File "FilterLists.scala" by KWR for CSE250, Spring 2022.
 *  Illustrates how the filter(...) method can work with lists.
 */
import io.StdIn._
import io.Source
import java.io.File         //technically not needed
import java.io.FileWriter   //makes it easy to append
import java.io.PrintWriter  //makes "print" and "println" available

object FilterLists extends App {
   val ell = List(2,7,9,4,6,2,4)
   val ell2 = ell.filter(_ > 4)
   println("The filtered list of " + ell + " is " + ell2)

   //How does filter work?

   def myfilter[A](ell: List[A], f: A => Boolean): List[A] = {
      ell match {
         case Nil => Nil
         case x::xs => if (f(x)) { x::myfilter(xs,f) } else myfilter(xs,f)
      }
   }

   
   //println("The myfilter-ed list of " + ell + " is " + myfilter(ell2,_ > 4))
   def gt4(x:Int)= x > 4
   println("The myfilter-ed list of " + ell + " is " + myfilter(ell, gt4))
}
