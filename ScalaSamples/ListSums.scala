/** File "ListSums.scala", by KWR for CSE250, Spring 2022
 */

object ListSums extends App {

   def sumList(ell: List[Int]): Int = ell match {
      case Nil => 0
      case x :: xs => x + sumList(xs)
   }

   val ell2 = List(1,2,3,4,5)
   val x = sumList(ell2)
   println("The sum is " + sumList(ell2))

   def sumList(ell: List[Int], acc): Int = ell match {
      case Nil => 0
      case x :: xs => (x+acc) :: sumList(xs, x + a)
   }


}
