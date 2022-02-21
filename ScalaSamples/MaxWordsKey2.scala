/** File "MaxWordsKey2.scala" by KWR for CSE250, Spring 2022.
 *  Assignment 1 key using for-loops and variables
 */
import io.StdIn._
import io.Source
import java.io.File         //technically not needed
import java.io.FileWriter   //makes it easy to append
import java.io.PrintWriter  //makes "print" and "println" available

object MaxWordsKey2 extends App {
   val inputFile = if (args.length >= 1) args(0) else "words.txt"

   val matrix = Source.fromFile(inputFile).getLines().map(_.split("\\s+").filter(_.length >= 1)).toArray
   //val matrix = Source.fromFile(inputFile).getLines().map(_.split(" ")).toArray
   println(s"I read ${matrix.length} lines")   //optional optical debug

   var (maxLen,maxRow,maxCol,maxWord) = (-1,-1,-1,"")

   for (i <- matrix.indices) {
      val lineArray = matrix(i)       //does this copy the whole line, or just copy the reference?
      for (j <- lineArray.indices) {
         if (lineArray(j).length > maxLen) {
         //if (lineArray(j).length >= maxLen) {
            maxLen = lineArray(j).length
            maxRow = i
            maxCol = j
            maxWord = lineArray(j)
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

/* Essay: (only covering differences with the first version)
   The code with variables is simpler because it doesn't need two separate "max" methods.
   But for the same reason, it doesn't allow you to re-use a method for finding the max-length
   word in a line.  A better aspect of this code (IMPHO) compared to the tuples is that each
   quantity maxLen, maxRow, maxCol, and maxWord uses just one descriptive name.  Whereas in
   key 1 it gets aliased to a tuple field, which does not have a name (and you might get them mixed up).
*/

