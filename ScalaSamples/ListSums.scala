/** File "ListSums.scala", by KWR for CSE250, Spring 2022
 */

object ListSums extends App {

   /** Basic list recursion example just to generate a sum.
    */
   def sumList(lis: List[Int]): Int = lis match {
      case Nil => 0
      case x :: xs => x + sumList(xs)
   }

   val ell = List(1,2,3,4,5)
   val x = sumList(ell)
   println("The sum is " + sumList(ell))

   /** Generate not just the whole sum but all partial sums
       (1,2,3,4,5) => (1,3,6,10,15)
       by using the partial sum 'psum" as a so-called "accumulator parameter"
    */
   def prefSums1(lis: List[Int], psum: Int): List[Int] = (lis, psum) match {
      case (Nil, psum) => Nil
      case (x :: xs, psum) => (x+psum) :: prefSums1(xs, psum+x)
   }

   /** This one does not need the accumulator to be involved in the match
    */
   def prefSums2(lis: List[Int], psum: Int): List[Int] = lis match {
      case Nil => Nil
      case x :: xs => (x+psum) :: prefSums1(xs, psum+x)
   }

   val psumList1 = prefSums1(ell, 0)
   println("The list of prefix sums is " + psumList1)

   val psumList2 = prefSums2(ell, 0)
   println("The list of prefix sums is " + psumList2)


   /** Strings can be recursed on by matching on the option of whether they have
       a first character.  NOTE: This may be a reason Scala 2.13.x is required.
    */
   def explode(str: String): List[Char] = str.headOption match {
      case None => Nil
      case Some(c) => c :: explode(str.tail)
   }

   println("The word big-faced exploded is " + explode("big-faced"))

   /** Example of a match taking things two-at-a-time.  Count the number
       of decreasing steps in a numerical sequence.
    */
   def countDips(ell: List[Int]): Int = ell match {
      case Nil => 0
      case x :: Nil => 0
      //case x :: y :: rest => if (y < x) { 1 + countDips(y :: rest) } else { countDips(y :: rest) }
      case x :: y :: rest => (y < x).compare(false) + countDips(y :: rest)    //neater but riskier?
   }

   println(s"The number of dips in $ell is ${countDips(ell)} and in ${ell.reverse} is ${countDips(ell.reverse)}")
   
}
