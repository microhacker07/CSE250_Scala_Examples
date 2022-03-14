/** File Fibonacci.scala by KWR for CSE250, Spring 2022
    Show O(n) style recursion, contrasted with exponential blowup
 */

import scala.math._

object Fibonacci extends App {

   val ms = 1000000.0

   /** Return (F_n,F_{n-1})
       Assertion: this takes C*n time, where we'll find out what C is...
    */
   def smartFib(n: BigInt): (BigInt,BigInt) = {
      if (n <= 1) {
         return (1,1) 
      } else {
         val (fibnm2,fibnm3) = smartFib(n-2) //this takes C*(n-2) time by IH
         //This equals C*n - 2*C.  So if 2C is the time for the if-else test plus the next statement
         //then the time on this run will be <= C*n, thus carrying the induction.

         return (2*fibnm2 + fibnm3, fibnm2 + fibnm3) //unit time code statements
      }
   }
   
   val n = 45
   var t1 = System.nanoTime()
   val (fibn,fibnm1) = smartFib(n)
   var t2 = System.nanoTime()
   println(s"Fibonacci $n and ${n-1} are $fibn and $fibnm1 in time ${(t2-t1)/ms} msec.")

   def dumbFib(n: BigInt): BigInt = {
      if (n <= 1) {  //F_1 = 1 and F_0 = 1
         return 1
      } else {
         return dumbFib(n-1) + dumbFib(n-2)
      }
   }

   t1 = System.nanoTime()
   val fibnd = dumbFib(n)
   t2 = System.nanoTime()
   
   println(s"Fibonacci $n the long way is $fibnd in time ${(t2-t1)/ms} msec.")

   /** Fill in array w/o recursion
   */
   def nonrecFib(n: Int): Array[BigInt] = {
      var fibArray = Array(1:BigInt,1:BigInt)
      for (j <- 2 to n) {
         fibArray :+= fibArray(j-2) + fibArray(j-1)
      }
      return fibArray
   }

   t1 = System.nanoTime()
   val fibnnrec = nonrecFib(n).last
   t2 = System.nanoTime()

   println(s"Fibonacci $n the nonrecursive way is $fibnnrec in time ${(t2-t1)/ms} msec.")
}
   
