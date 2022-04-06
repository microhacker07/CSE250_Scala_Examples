/** File "MaxWordsKey1.scala" by KWR for CSE250, Spring 2022.
 *  Assignment 2 key using tuples and functions---no global data
 *  Uses "return" keyword now.
 */
import io.StdIn._
import io.Source
import java.io.File         //technically not needed
import java.io.FileWriter   //makes it easy to append
import java.io.PrintWriter  //makes "print" and "println" available

object MaxWordsKey1 extends App {
   val inputFile = if (args.length >= 1) args(0) else "words.txt"

   val dictFile = "../wordsDWYL.txt"

   val aol = Source.fromFile(inputFile).getLines().map(line => tokenize(line)).toArray

   //val dict = Source.fromFile(dictFile).getLines().toSet
   val dict = Source.fromFile(dictFile).getLines().map(_.toLowerCase).toSet
   println("Read " + dict.size + " words into the dictionary")

   def maxLengthAndIndexInRow(arr:List[String]):(Int,Int,String) = {
      var maxTuple = (-1,-1,"")        //use out-of-bounds -1 as default for non-neg. quantities
 
      for (j <- arr.indices) {
         if (arr(j).length > maxTuple._1) {
            maxTuple = (arr(j).length,j,arr(j))
         }
      }
      return maxTuple
   }

   def maxLengthAndLineAndIndex(arr2:Array[List[String]]):(Int,Int,Int,String) = {
      var maxTuple = (-1,-1,-1,"")
      for (i <- arr2.indices) {
         val (len,j,word) = maxLengthAndIndexInRow(arr2(i))
         if (len > maxTuple._1) {
         //if (len >= maxTuple._1) {                  //preserves last topper of ties
            maxTuple = (len,i,j,word)
            println(s"\nNew top word and line: $word in " + arr2(i).toList)
         }
      }
      return maxTuple
   }

   /** Convert to list with alphanumeric plus ' tokens, else singleton items
       INV: accumulator parameter "alphanum" is an alphanumeric string
    */
   def tokenize(line: String, alphanum: String = ""): List[String] = (line.headOption, alphanum) match {
      case (None, "") => Nil
      case (None, s) => s :: Nil
      case (Some(c), s) => if (c.isLetterOrDigit || c == '\'') {
         tokenize(line.tail, s + c)
      } else if (c.isWhitespace) {
         if (s == "") tokenize(line.tail, s) else s::tokenize(line.tail, "")
      } else {
         if (s == "") {
            ("" + c) :: tokenize(line.tail, "")
         } else {
            s :: ("" + c) :: tokenize(line.tail, "")
         }
      }
   }

   /** Find removable hyphens within each line of text.  The removal cases, in order:
       1. "battle-field" should become "battlefield", and "vis-a" should become "visa"
       2. "horse-buggy" should become "horse" then "buggy" since "horsebuggy" is not a word but
          "horse" and "buggy" are words separately.
       3. "slo-mo" should be left as it is, since neither "slomo" nor "slo" is in the dictionary.
    */
   def dehyphenate(ell: List[String]): List[String] = ell match {

      case word1 :: "-" :: word2 :: rest => if (dict(word1 + word2)) {
         println("Dehyphenating " + word1 + "-" + word2 + " to " + word1 + word2)
         //dehyphenate((word1 + word2) :: rest)
         (word1 + word2) :: dehyphenate(rest)    //fine too

      } else if (dict(word1) && dict(word2)) {
         println("Dehyphenating " + word1 + "-" + word2 + " to " + word1 + " " + word2)
         //dehyphenate(word1 :: word2 :: rest)    //OK
         //word1 :: dehyphenate(word2 :: rest)      //maybe best
         word1 :: word2 :: dehyphenate(rest)    //ditto, but misses some double-hyphen opportunities
      } else {
         println("Leaving " + word1 + "-" + word2 + " alone.")
         //dehyphenate((word1 + "-" + word2) :: rest)
         (word1 + "-" + word2) :: dehyphenate(rest)
      }

      //next case means word3 not followed by hyphen, so just take it and move on.
      case word3 :: rest => word3 :: dehyphenate(rest)
      case Nil => Nil
   }

   //Rest of client is same as before.

   val (maxLen2,maxRow2,maxCol2,maxWord2) = maxLengthAndLineAndIndex(aol.map(ell => dehyphenate(ell)))
   val out = s"The word of longest length $maxLen2 is $maxWord2 in line $maxRow2, column $maxCol2"
   println("After tokenizating and dehyphenating:\n" + out)
   val filep = new PrintWriter(new FileWriter("output.txt",true));  //appends
   filep.println("After tokenizating and dehyphenating:\n" + out)

   filep.close()

}


/* Essays put here for convenience.
   Q1: This version of "dehyphenate" takes hyphenated chains two-at-a-time.  This means that on

      foo-bar-delta

   it will first apply the rules for "foo-bar".  Then depending on which removal case applies, it will do
   1. foobar-delta ...
   2. foo :: bar-delta ...
   3. "foo-bar"-delta with "foo-bar" now treated as one word.
   So on "comical-historical-pastoral" in JustHamlet.txt, it will give
   comical :: historical :: pastoral
   But e.g. on "consecrate--" in Gettysburg.txt, what happens is that the case-1 test does not find
   "consecrate-" in the dictionary, so it goes to case 2.  This finds "consecrate" in the
   dictionary, but "-" is not.  So "consecrate--" gets left as one word via case 3.  Which is
   maybe not ideal.  (It does then get reported as the longest word in Gettysburg.txt.)

   Of course, YMMV and that is fine.

   One other note: In cases of two dashes between words, as in "devotion--that" in Gettysburg.txt,
   it leaves "devotion--" as one block and works on "devotion--" :: that :: ...
   Because "that" is not a hyphen, it puts "devotion--" onto the output list and works
   recursively on "that".  Thus those two parts get separated.  But the triple hyphen "cases---print"
   in SmallTest.txt alas reappears like a zombie because "cases--" :: "-" :: "print" matches the
   operative first case with a hyphen again.
   

   Q2: The original dictionary has 466,551 words.  When all words are lowercased, it becomes
   466,547 words.  That is, it shrinks by just 4 words!  (Hence the Q reading "if at all"...)
   The words happen to be AS,as,Dino,dino,The,the,TO,to.  The reason it shrinks is that (unlike List
   and Array) the Set datatype does not allow duplicates.  
*/
   


