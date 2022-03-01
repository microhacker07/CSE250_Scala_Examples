/** File "ListSums.scala", by KWR for CSE250, Spring 2022
 */

object ListSums extends App {

   /** Basic list recursion example just to generate a sum.
    */
   def sumList(lis: List[Int]): Int = lis match {
      case Nil => 0
      case x :: xs => x + sumList(xs)
   }

   val ell = List(1,2,3,4,5,6,7)
   val x = sumList(ell)
   println("The sum is " + sumList(ell))


   /** Generate not just the whole sum but all partial sums,
       e.g. (1,2,3,4,5,6,7) => (1,3,6,10,15,21,28),
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
      case x :: xs => (x+psum) :: prefSums2(xs, psum+x)
   }

   val psumList1 = prefSums1(ell, 0)
   println("The list of prefix sums is " + psumList1)

   val psumList2 = prefSums2(ell, 0)
   println("The list of prefix sums is " + psumList2)


   /** Example of the accumulator parameter used to keep track of "State".
       Here the state is whether a sum is about to go over a given threshold, in
       which case we put the new sum on the output stream and start again from zero.
       When the list ends, we finish by putting down whatever sum we have, if it's nonzero.
       Here it is (IMPHO) cleaner to include the test psum =? 0 in the match.
       Convention is to put accumulator parameters(s) last.
    */
   def thresholdSums0(ell: List[Int], threshold: Int, psum: Int): List[Int] = (ell,psum) match {
      case (Nil,0) => Nil         // no more elements to add
      case (Nil,p) => p :: Nil    // psum :: Nil would be equivalent
      case (x :: xs, p) => if (x+p >= threshold) {
         (x+p) :: thresholdSums(xs, threshold, 0)
      } else {
         thresholdSums(xs, threshold, p+x)    // INVARIANT: psum < threshold in any call
      }
   }

   def thresholdSums(ell: List[Int], threshold: Int, psum: Int): List[Int] = ell match {
      case Nil => if (psum == 0) Nil else (psum :: Nil)         // no more elements to add
      case x :: xs => if (x+psum >= threshold) {
         (x+psum) :: thresholdSums(xs, threshold, 0)
      } else {
         thresholdSums(xs, threshold, psum+x)    // INVARIANT: psum < threshold in any call
      }
   }


   println("Sub-sums of " + ell + " for threshold 10 are " + thresholdSums(ell, 10, 0))
            


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

   /** We can take things 3 at a time and count cases of two consecutive dips.
       We have to decide whether the cases can overlap.
    */
   def count2Dips(ell: List[Int]): Int = ell match {
      case Nil => 0
      case x :: Nil => 0
      case x :: y :: Nil => 0    //two elements cannot make a double-dip
      case x :: y :: z :: rest => (y < x && z < y).compare(false) + count2Dips(y :: z :: rest) 
         //counts overlaps because we put both y and z back on the list being recursed on.

      //case x :: y :: z :: rest => (y < x && z < y).compare(false) + count2Dips(z :: rest)    
         //effectively skips overlaps because we don't put y back, only z
   }

   val ell2 = List(1,3,6,2,5,4,2,1,9,6,4,8)
   println("Sub-sums of " + ell2 + " for threshold 10 are " + thresholdSums(ell2, 10, 0))
   println("Its number of dips is " + countDips(ell2) + ", and double-dips (with overlaps) is " + count2Dips(ell2))


   /** Now show maximal sequences of decreasing-length words in a list of strings.
       Using accumulators for both the phrase and the length enables using the simplest kind of match.
       Because the first word will always begin a new chain, we want to begin with a super-high length #
    */
   def decreasingPhrases(words: List[String], phrase: String, lastWordLen: Int): List[String] = words match {
      case Nil => if (phrase == "") Nil else phrase::Nil
      case wd :: rest => if (wd.length >= lastWordLen) {   //decreasing phrase stopped, wd may begin new phrase
         phrase :: decreasingPhrases(rest, wd, wd.length) 
      } else if (phrase == "") {
         decreasingPhrases(rest, wd, wd.length)
      } else {
         decreasingPhrases(rest, phrase + " " + wd, wd.length)
      }
   }

   val line = "The quick brown fox jumped over the lazy dog's back"   
   val decreasingPhraseList = decreasingPhrases(line.split(" ").toList, "", 9999)
   println("Decreasing phrases, including single ones: " + decreasingPhraseList)
   
}
