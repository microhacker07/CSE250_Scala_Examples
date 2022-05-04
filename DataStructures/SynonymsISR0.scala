/** File "SynonymsISR0.scala", by KWR for CSE250, Spring 2022
    Client for tests involving Samuel Fallows's 1898 book of synonyms and antonyms.
    Uses the ISR library with interchangeable implementations.  So far releasing:
    SortedSLL, SortedDLL (both sloooow), SortedArray (fastest), BALBOA, BALBOADLL (similar)
    REQUIRES compiled versions of these classes in same folder, plus ISR and Cardbox

    (Incorporates the added line with ".trim()" allowing both versions of Fallows1898 to be read.
    But the official version /.../cse250/DataStructures/Fallows1898 now has important changes,
    so it is the only recognized copy of the file to work from.)
 */

import io.StdIn._
import io.Source
import java.io.File         //technically not needed
import java.io.FileWriter   //makes it easy to append
import java.io.PrintWriter  //makes "print" and "println" available

import scala.collection.mutable.ArrayBuffer   //only for initial read of fallows1898 into "synarray"



/** Same simple class as in the Map-based Assignment 4 key, but truly needed for ISR use.
    On Assignment 5, CHANGE THIS to have an extra field for the part of speech.
 */
case class SynonymEntry0(key: String, synonyms: StringBox0)


//We use ISR classes for *both* the individual synonym lists *and* the whole book 

class StringBox0 extends Cardbox[String]((x,y) => x.compareTo(y))   //LEAVE THIS THE SAME

class SynonymBox0 extends Cardbox[SynonymEntry0]((x,y) => x.key.compareTo(y.key)) {

   /** By coding "apply" we can automatically adapt the "contains" already coded
       in ISR.scala to work with the exact same syntax used for Scala's own Map class.
       
       BUT YOU WILL ELIMINATE THIS MAP-LIKE CODE, USING ITERATORS INSTEAD.
       COMMENT-IN THE "LINE TO GET YOU STARTED" BELOW AND WORK FROM THERE
    */
   def apply(key: String): StringBox0 = {
      val itr = find(SynonymEntry0(key, new StringBox0()))
      if (itr.hasNext) {
         return itr().synonyms
      } else {
         println("SynonymBox0.apply(key) used on non-present key, hope returning empty synonyms list is OK.")
         return new StringBox0()
      }
   }
}



/** Principal lines begin "KEY: " and "SYN: ", the latter possibly
    followed by similar lines with no headword and terminated by a line
    beginning "ANT:" or with "=".  Assume words of those lines begin in
    column 5.  Reader uses state pattern to work one line at a time.
 */
object SynonymReader0 {
   def readEntries: ArrayBuffer[SynonymEntry0] = {
      //val synFile = "Fallows1898.txt"
      val synFile = "Fallows1898fx.txt"        //now use this version of the file
      val src = Source.fromFile(synFile)
      var synarray = new ArrayBuffer[SynonymEntry0]()
   
      var key = ""
      var inSyn = false
      var accumeLine = ""
      var count = 0
      for (line <- src.getLines()) {
         if (line.startsWith("KEY:")) {
            if (key != "" || inSyn) {
               println("Parse off rails at " + line)
            }
            val restLine = line.substring(5)
            var inAlpha = true
            for (c <- restLine if inAlpha) {
               if (c.isLetter || c == '_' || c == '-') {
                  key += c
               } else {
                  inAlpha = false
               }
            }
            if (key != "") {
               key = key.toLowerCase

               //USE EXTRA SynonymEntry FIELD INSTEAD OF UGLY SUFFIX TAG

               if (line.contains("\\n.\\")) {
                  key += "_n__"
               } else if (line.contains("\\v.\\")) {
                  key += "_v__"
               } else if (line.contains("\\a.\\")) {
                  key += "_a__"
               } else if (line.contains("\\r.\\")) {
                  key += "_r__"
               }
            }
         }
         else if (line.startsWith("SYN:")) {
            if (key == "") {
               println("No key found for synonyms beginning " + line)
            } else if (inSyn) {
               println("Parse off rails at " + key + ":" + line)
            }
            inSyn = true
            accumeLine = line.substring(5)
   
         } else if (line.startsWith("ANT:") || line.startsWith("=")) {
            if (inSyn && key != "") {
               if (accumeLine.endsWith(".")) { accumeLine = accumeLine.dropRight(1) }
               val syns = accumeLine.split(",\\s+")
               if (syns.size > 0) {

                  //!! Need "insert" not "+=" to keep sortedness with the ISR code.
                  val item = SynonymEntry0(key, new StringBox0())
                  for (word <- syns) {
                     //item.synonyms += (word.toLowerCase)
                     item.synonyms.insert(word.toLowerCase)
                  }
                  synarray :+= item
                  count += 1
                  if (count % 1000 == 0) {
                     println(s"Entry $count is ${item.key}:" + item.synonyms.toList)
                  }
               } else {
                  println("Empty synonym list found for key " + key)
               }
            }
            key = ""
            inSyn = false
            accumeLine = ""
         } else if (inSyn) {
            accumeLine += " " + line
         } else {
            //do nothing
         }
      }
   
      return synarray
            
   }
}
            
            
object SynonymsISR0 extends App {
   val outp = new PrintWriter(new FileWriter("output.txt",true));  //appends

   var synarray = SynonymReader0.readEntries
   println("Created " + synarray.length + " entries.")

   val lookup = new SynonymBox0()           //ISR for both items and their synonym lists

   var count = 0
   val ms = 1000000.0
   val allowPrintWhenTiming = true

   println("\n\n\n\n")
   println("Timing the creation of the SynonymsBox, reading simple entries from array...")

   val tm1 = System.nanoTime()
   for (item <- synarray) {
      count = lookup.size

      lookup.insert(item)

      if (lookup.size == count && allowPrintWhenTiming) {
         println("SynonymBox0 did not increase when adding " + item.key + ": " + item.synonyms.toList)
      }
      if (count % 1000 == 0 && allowPrintWhenTiming) {
         println(s"SynonymBox0 item $count is ${item.key}:" + item.synonyms.toList)
      }
   }
   val tm2 = System.nanoTime()
   var elapsedTime = (tm2 - tm1)/ms
   println("")
   println("SynonymBox0 creation took time " + elapsedTime + " ms, from " + lookup.size + " different entries")
   println("\n\n\n\n")

   /** Needed because "find" etc. take a whole item, not just its key.  
       But it can be a dummy item in fields that aren't used for keys.
       Note that "null.asInstanceOf[StringBox0]" is OK even when StringBox0 is a case class.
       SLIGHT CHANGE TO THIS
    */
   def dummyEntry(key: String) = new SynonymEntry0(key, new StringBox0())


   println("\nTiming the main run now..." + (if (allowPrintWhenTiming) "" else "no printing...") + "\n")
   
   val t1 = System.nanoTime()

   //for ((key,synSet) <- lookup) {   //ISR uses explicit iteration instead.

   //TASK: For each synonym "word" of key, lookup word and see if key is
   //one of word's own synonyms.  Because word might appear in different
   //noun,verb,adj., etc. forms, we have to try multiple items having the same key.
   //Since the ISR implementations are sorted, these items will be consecutive for the iterator.

   var entryitr = lookup.begin
   while (entryitr.hasNext) {
      val entry = entryitr.next()
      val key = entry.key   //just so no need to retype next lines
      val synSet = entry.synonyms
      val kw = if (key.endsWith("__")) key.dropRight(4) else key    //YOU WILL NOT NEED THIS KLUDGE
      val tag = if (key.endsWith("__")) " ("+kw+"="+key.takeRight(4)(1)+")" else ""
      var synitr = synSet.begin
      while (synitr.hasNext) {
         val word = synitr.next()
         //var worditr = lookup.find(dummyEntry(word))   //LINE TO GET YOU STARTED

         //THE FOLLOWING IS AD-HOC/KLUDGEY.  USE DUPLICATE-KEY ITERATION TO SIMPLIFY IT
         val recip = ((lookup.contains(dummyEntry(word)) && lookup(word).contains(kw))
                        || (lookup.contains(dummyEntry(word+"_v__")) && lookup(word+"_v__").contains(kw))
                        || (lookup.contains(dummyEntry(word+"_n__")) && lookup(word+"_n__").contains(kw))
                        || (lookup.contains(dummyEntry(word+"_a__")) && lookup(word+"_a__").contains(kw))
                        || (lookup.contains(dummyEntry(word+"_r__")) && lookup(word+"_r__").contains(kw)))
         
         val hasSynonyms = ((lookup.contains(dummyEntry(word)) && lookup(word).size > 0)
                        || (lookup.contains(dummyEntry(word+"_v__")) && lookup(word+"_v__").size > 0)
                        || (lookup.contains(dummyEntry(word+"_n__")) && lookup(word+"_n__").size > 0)
                        || (lookup.contains(dummyEntry(word+"_a__")) && lookup(word+"_a__").size > 0)
                        || (lookup.contains(dummyEntry(word+"_r__")) && lookup(word+"_r__").size > 0))

         //YOU CAN IGNORE/DELETE THIS TEST CONDITION (it was my own debug check of the Fallows1898.txt file)
         val emptyKeyOnly = (!recip) && (!hasSynonyms) && (lookup.contains(dummyEntry(word))
                            || lookup.contains(dummyEntry(word+"_v__")) || lookup.contains(dummyEntry(word+"_n__"))
                            || lookup.contains(dummyEntry(word+"_a__")) || lookup.contains(dummyEntry(word+"_r__")))

         if (allowPrintWhenTiming) {
            if (recip && kw.startsWith("q")) {
               outp.println(s"$kw and $word are reciprocal synonyms" + tag)
               //println(s"$kw and $word are reciprocal synonyms"+tag)
            } else if ((!recip) && hasSynonyms && kw.startsWith("q")) {
               outp.println(s"$kw lists $word but $word has a list of synonyms without $kw")
               //println(s"$kw lists $word but $word has a list of synonyms without $kw")
            } else if (emptyKeyOnly) {
               //println(s"$kw lists $word which is a key but Xref only or otherwise has no synonyms")
            } else if (kw.startsWith("q")) {
               //println(s"$kw lists $word but that is not a key")
            }
         }
      }
   }
               
   val t2 = System.nanoTime()
   elapsedTime = (t2 - t1)/ms
   println(s"Elapsed time in milliseconds: $elapsedTime")
   outp.close()
}
   
   
