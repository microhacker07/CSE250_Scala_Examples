/** File "MaxWordsKey1.scala" by KWR for CSE250, Spring 2022.
 *  Assignment 1 key using tuples and functions---no global data
 */
import io.StdIn._
import io.Source
import java.io.File         //technically not needed
import java.io.FileWriter   //makes it easy to append
import java.io.PrintWriter  //makes "print" and "println" available

object MaxWordsKey1 extends App {
   val inputFile = if (args.length >= 1) args(0) else "words.txt"

   val matrix = Source.fromFile(inputFile).getLines().map(_.split("\\s+").filter(_.length >= 1)).toArray
   //val matrix = Source.fromFile(inputFile).getLines().map(_.split("\\s+")).toArray
   println(s"I read ${matrix.length} lines")   //optional optical debug

   def maxLengthAndIndexInRow(arr:Array[String]):(Int,Int,String) = {
      var maxTuple = (-1,-1,"")        //use out-of-bounds -1 as default for non-neg. quantities
 
      for (j <- arr.indices) {
         if (arr(j).length > maxTuple._1) {
            maxTuple = (arr(j).length,j,arr(j))
         }
      }
      maxTuple
   }

   def maxLengthAndLineAndIndex(arr2:Array[Array[String]]):(Int,Int,Int,String) = {
      var maxTuple = (-1,-1,-1,"")
      for (i <- arr2.indices) {
         val (len,j,word) = maxLengthAndIndexInRow(arr2(i))
         if (len > maxTuple._1) {
         //if (len >= maxTuple._1) {                  //preserves last topper of ties
            maxTuple = (len,i,j,word)
            println(s"\nNew top word and line: $word in " + arr2(i).toList)
         }
      }
      maxTuple
   }

   val (maxLen,maxRow,maxCol,maxWord) = maxLengthAndLineAndIndex(matrix)

   val filep = new PrintWriter(new FileWriter("output.txt",true));  //appends
   filep.println(s"The word of longest length $maxLen is $maxWord in line $maxRow, column $maxCol")
   filep.close()
   //echo to screen too
   println(s"The word of longest length $maxLen is $maxWord in line $maxRow, column $maxCol")

}

/* Essay: Lists are not used per-se.  But in the code where lines are read, split, and filtered, the 
   lines originally form an IterableSequence[String], which gets converted to an
   IterableSequence[Array[String]] by the map-of-split-and-then-filter.  The final .toArray call
   converts this to the ultimate Array[Array[String]] which the methods work on.
   [Confession: For sundry reasons I thought .split(...) produced a List[String] and .filter worked off that,
   seemingly confirmed by "quizzes", "assignments", and "tests" being Lists on page 54, but when
   that example begins on pages 35-36 they are arrays.  Then the essay would have a more affirmative answer.]
   The split on \s+ removes internal whitespeace but leaves an initial empty string on indented lines,
   which is why the call to filter(str => str.length >= 1) is needed to remove them.
   The first of two similar methods finds the longest word in a line and its place within the line.
   The second compares lines according to the length of their longest word.
   Using methods produces code with no top-level int or string "global state"variables.
   There remains the question of handling ties.  With the > comparison in both places, the first longest
   word in a line with two or more equal-length longest words is preserved, and the earlies line with
   a word of that length is returned.  A >= comparison in the latter would give output from the latest
   line with a word of the same maximum length, but still take the first such word in the line because the 
   "in-row" comparison stayed with the > comparison.  The file Declaration.txt, now with the extra
   word "Uncopyrightable" added to the last line, illustrates these points.
*/
