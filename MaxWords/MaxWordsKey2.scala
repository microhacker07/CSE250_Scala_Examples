/** File "MaxWordsKey2.scala" by KWR for CSE250, Spring 2022.
 *  Assignment 2, second key using for-loops and variables
 */
import io.StdIn._
import io.Source
import java.io.File         //technically not needed
import java.io.FileWriter   //makes it easy to append
import java.io.PrintWriter  //makes "print" and "println" available

object MaxWordsKey2 extends App {
   val inputFile = if (args.length >= 1) args(0) else "words.txt"

   val dictFile = "../wordsDWYL.txt"

   var acc = ""   //The reason why this is needed here, not below next to "tokenbize", is INSANE
   val matrix = Source.fromFile(inputFile).getLines().map(line => tokenize(explode(line))).toArray

   //val dict = Source.fromFile(dictFile).getLines().toSet
   val dict = Source.fromFile(dictFile).getLines().map(_.toLowerCase).toSet
   println("Read " + dict.size + " words into the dictionary")


   def explode(str: String): List[Char] = str.headOption match {
      case None => Nil
      case Some(c) => c :: explode(str.tail)
   }


   /** Convert to list with alphanumeric plus ' tokens, else singleton items
       INV: global accumulator variable acc is an alphanumeric string
    */
   //var acc = ""  
   def tokenize(charList: List[Char]): List[String] = {
      //var acc = ""
      return charList match {
         case Nil => if (acc == "") Nil else {
            val tmp = acc
            acc = ""
            tmp :: Nil
         }
         case x :: xs => if (x.isLetterOrDigit || x == '\'') {
            acc += x
            tokenize(xs)
         } else if (x.isWhitespace) {
            if (acc == "") {
               tokenize(xs)
            } else {
               val tmp = acc  //kludgey but needed here
               acc = ""
               tmp :: tokenize(xs)
            }
         } else {
            if (acc == "") {
               (""+x) :: tokenize(xs)
            } else {
               val tmp = acc  //kludgey but needed here too
               acc = ""
               tmp :: (""+x) :: tokenize(xs)
            }
         }
      }
   }

   //val matrix = Source.fromFile(inputFile).getLines().map(line => tokenize(explode(line))).toArray


   /** Find removable hyphens within each line of text.  The removal cases, in order:
       1. "battle-field" should become "battlefield", and "vis-a" should become "visa"
       2. "horse-buggy" should become "horse" then "buggy" since "horsebuggy" is not a word but
          "horse" and "buggy" are words separately. 
       3. "slo-mo" should be left as it is, since neither "slomo" nor "slo" is in the dictionary.
    */
   var savedWord = ""  //global temp var, may end with -

   def dehyphenate(ell: List[String]): List[String] = ell match {
      case Nil => if (savedWord == "" || savedWord == "-") Nil else {
         val tmp = savedWord
         savedWord = ""
         if (tmp.last == '-') (tmp.init)::"-"::Nil else tmp::Nil
      }

      case "-" :: rest => savedWord += "-"; dehyphenate(rest)

      case word2 :: rest => if (savedWord == "") {
         savedWord = word2
         dehyphenate(rest)
      } else if (savedWord.last == '-') {
         val word1 = savedWord.init
         savedWord = word2 //reset for recursive call
         if (dict.contains(word1 + word2)) {
            println("Dehyphenating " + word1 + "-" + word2 + " to " + word1 + word2)
            dehyphenate((word1 + word2) :: rest)
         } else if (dict.contains(word1) && dict.contains(word2)) {
            println("Dehyphenating " + word1 + "-" + word2 + " to " + s"$word1 $word2")
            dehyphenate(word1 :: word2 :: rest)
         } else {
            println("Leaving " + word1 + "-" + word2 + " alone.")
            dehyphenate((word1 + "-" + word2) :: rest)
         }
      } else {    //no hyphen, just swap saved word
         val word1 = savedWord
         savedWord = word2 //reset for recursive call
         word1 :: dehyphenate(rest)
      }
   }


   // Main body of client code similar to before

   var (maxLen,maxRow,maxCol,maxWord) = (-1,-1,-1,"")

   for (i <- matrix.indices) {
      val lineList = dehyphenate(matrix(i))       //does this copy the whole line, or just copy the reference?
      for (j <- lineList.indices) {
         if (lineList(j).length > maxLen) {
         //if (lineList(j).length >= maxLen) {
            maxLen = lineList(j).length
            maxRow = i
            maxCol = j
            maxWord = lineList(j)
            println(s"\nNew top word and line: $maxWord in " + matrix(i).toList)
         }
      }
   }


   val filep = new PrintWriter(new FileWriter("output.txt",true));  //appends
   filep.println(s"The word of longest length $maxLen is $maxWord in line $maxRow, column $maxCol")
   filep.close()
   //echo to screen too
   println(s"The word of longest length $maxLen is $maxWord in line $maxRow, column $maxCol")

}

/* Essays put here for convenience.
   Q1: This version of "dehyphenate" also takes hyphenated chains two-at-a-time.  This means that on

      foo-bar-delta

   it will first apply the rules for "foo-bar".  Then depending on which removal case applies, it will do
   1. foobar-delta ...
   2. foo :: bar-delta ...
   3. "foo-bar"-delta with "foo-bar" now treated as one word.
   So on "comical-historical-pastoral" in JustHamlet.txt, it will give
   comical :: historical :: pastoral

   But there is one meaningful difference on how it handles multiple hyphens that are consecutive,
   i.e., dashes.  In case of "devotion--that" in Gettysburg.txt, it repeats the case option for hyphen

   case "-" :: rest

   for both hyphens in a row, without trying the other code.  Thus both hyphens get tacked on to
   the savedWord variable.  That makes "devotion--that" be kept as a unit, and makes it be
   reported as the longest word in Gettysburg.txt.  This is instead of "consecrate--" in my
   "MaxWordsKey1.scala".

   Of course, YMMV is fine---and frankly, probably neither of my keys is handling cases of
   two *consecutive* hyphens in the best way.  And both screw up on *three* consecutive hyphens,
   i.e. on "cases---print" in SmallTest.txt.  (Understanding this, not being perfect, is the goal.)


   Q2: The original dictionary has 466,551 words.  When all words are lowercased, it becomes
   466,547 words.  That is, it shrinks by just 4 words!  (Hence the Q reading "if at all"...)
   The words happen to be AS,as,Dino,dino,The,the,TO,to.  The reason it shrinks is that (unlike List
   and Array) the Set datatype does not allow duplicates.
*/

