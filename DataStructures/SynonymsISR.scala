/** File "SynonymsISR.scala", by KWR for CSE250, Spring 2022
    Client for tests involving Samuel Fallows's 1898 book of synonyms and antonyms.
    Uses the ISR library with interchangeable implementations.  So far releasing:
    SortedSLL, SortedDLL (both sloooow), SortedArray (fastest), BALBOA, BALBOADLL (similar)
    REQUIRES compiled versions of these classes in same folder, plus ISR and Cardbox
 */

import io.StdIn._
import io.Source
import java.io.File         //technically not needed
import java.io.FileWriter   //makes it easy to append
import java.io.PrintWriter  //makes "print" and "println" available

import scala.collection.mutable.ArrayBuffer


//import ISR._   //still don't have packages working on Autograder... :-(


/** Same simple class as in the Map-based Assignment 4 key, but truly needed for ISR use.
 */
case class SynonymEntry(key: String, synonyms: StringBox)



//Use ISR classes for *both* the individual synonym lists *and* the whole book 

//class StringBox extends Cardbox[String]((x,y) => x.compareTo(y))      //doesn't matter here
case class StringBox() extends Cardbox[String]((x,y) => x.compareTo(y))

class SynonymBox extends Cardbox[SynonymEntry]((x,y) => x.key.compareTo(y.key)) {

   /** By coding "apply" we can automatically adapt the "contains" already coded
       in ISR.scala to work with the exact same syntax used for Scala's own Map class.
    */
   def apply(key: String): StringBox = {
      //val itr = find(new SynonymEntry(key, new StringBox())) 
      val itr = find(SynonymEntry(key, StringBox()))
      if (itr.hasNext) {
         return itr().synonyms
      } else {
         println("SynonymBox.apply(key) used on non-present key, hope returning empty synonyms list is OK.")
         //return new StringBox()
         return StringBox()
      }
   }
}



/** Principal lines begin "KEY: " and "SYN: ", the latter possibly
    followed by similar lines with no headword and terminated by a line
    beginning "ANT:" or with "=".  Assume words of those lines begin in
    column 5.  Reader uses state pattern to work one line at a time.
    SAME as in the Map-based answer key, except for 2 lines as noted.
 */
object SynonymReader {
   def readEntries: ArrayBuffer[SynonymEntry] = {
      val synFile = "Fallows1898.txt"
      val src = Source.fromFile(synFile)
      var synarray = new ArrayBuffer[SynonymEntry]()
   
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
               if (line.contains("\\n.\\")) {
                  //println("Added noun from line " + line)
                  key += "_n__"
               } else if (line.contains("\\v.\\")) {
                  //println("Added verb from line " + line)
                  key += "_v__"
               } else if (line.contains("\\a.\\")) {
                  //println("Added adjective from line " + line)
                  key += "_a__"
               } else if (line.contains("\\r.\\")) {
                  //println("Added adjective from line " + line)
                  key += "_r__"
               }
               //if (key.startsWith("wage")) { println(key) }
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
                  //val item = new SynonymEntry(key, Set[String]())
                  //val item = SynonymEntry(key, SortedSet[String]())
                  //val item = SynonymEntry(key, ListBuffer[String]())
                  //ONLY CHANGES are here.  Need "insert" not "+=" to keep sortedness.
                  val item = SynonymEntry(key, new StringBox())
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
            
            
object SynonymsISR extends App {
   val outp = new PrintWriter(new FileWriter("output.txt",true));  //appends

   var synarray = SynonymReader.readEntries
   println("Created " + synarray.length + " entries.")

   //val lookup = SortedMap.empty[String,SortedSet[String]]   //A4 key choice
   //val lookup = SortedMap.empty[String,StringBox]           //halfway ISR use choice
   val lookup = new SynonymBox()                              //ISR for both items and their synonym lists

   var count = 0
   val ms = 1000000.0
   val allowPrintWhenTiming = true

   println("\n\n\n\n")
   println("Timing the creation of the SynonymsBox, reading simple entries from array...")
   if (allowPrintWhenTiming) {
      println("...allowing printing to screen & file.  Note: twenty inserts put in duplicate keys...")
      println("...because Fallows1898.txt has 20 buggy repeated entries of words, clearly typos...\n")
   }

   val tm1 = System.nanoTime()
   for (item <- synarray) {
      count = lookup.size

      //lookup(item.key) = item.synonyms
      //lookup += (item.key -> item.synonyms)
      lookup.insert(item)

      if (lookup.size == count && allowPrintWhenTiming) {
         println("SynonymBox did not increase when adding " + item.key + ": " + item.synonyms.toList)
      }
      if (count % 1000 == 0 && allowPrintWhenTiming) {
         println(s"SynonymBox item $count is ${item.key}:" + item.synonyms.toList)
      }
   }
   val tm2 = System.nanoTime()
   var elapsedTime = (tm2 - tm1)/ms
   println("")
   println("SynonymBox creation took time " + elapsedTime + " ms, from " + lookup.size + " different entries")
   println("\n\n\n\n")

   /** Needed because "find" etc. take a whole item, not just its key.  
       But it can be a dummy item in fields that aren't used for keys.
       Note that "null.asInstanceOf[StringBox]" is OK even when StringBox is a case class.
    */
   //def dummyEntry(key: String) = new SynonymEntry(key, new StringBox())
   def dummyEntry(key: String) = new SynonymEntry(key, null.asInstanceOf[StringBox])
   //def dummyEntry(key: String) = new SynonymEntry(key, StringBox())


   println("\nTiming the main run now..." + (if (allowPrintWhenTiming) "" else "no printing...") + "\n")
   
   val t1 = System.nanoTime()

   //for ((key,synSet) <- lookup) {   //ISR uses explicit iteration instead.
   var eitr = lookup.begin
   while (eitr.hasNext) {
      val entry = eitr.next()
      val key = entry.key   //just so no need to retype next lines
      val synSet = entry.synonyms
      val kw = if (key.endsWith("__")) key.dropRight(4) else key
      val tag = if (key.endsWith("__")) " ("+kw+"="+key.takeRight(4)(1)+")" else ""
      var itr = synSet.begin
      //for (word <- synSet) {
      while (itr.hasNext) {
         val word = itr.next()
         val recip = ((lookup.contains(dummyEntry(word)) && lookup(word).contains(kw))
                        || (lookup.contains(dummyEntry(word+"_v__")) && lookup(word+"_v__").contains(kw))
                        || (lookup.contains(dummyEntry(word+"_n__")) && lookup(word+"_n__").contains(kw))
                        || (lookup.contains(dummyEntry(word+"_a__")) && lookup(word+"_a__").contains(kw))
                        || (lookup.contains(dummyEntry(word+"_r__")) && lookup(word+"_r__").contains(kw)))
         //recip = (lookup.contains(word) && lookup(word).contains(kw))
         val nonrecip = (!recip) && ((lookup.contains(dummyEntry(word)) && lookup(word).size > 0)
                        || (lookup.contains(dummyEntry(word+"_v__")) && lookup(word+"_v__").size > 0)
                        || (lookup.contains(dummyEntry(word+"_n__")) && lookup(word+"_n__").size > 0)
                        || (lookup.contains(dummyEntry(word+"_a__")) && lookup(word+"_a__").size > 0)
                        || (lookup.contains(dummyEntry(word+"_r__")) && lookup(word+"_r__").size > 0))
         val emptyKeyOnly = (!recip) && (!nonrecip) && (lookup.contains(dummyEntry(word))
                            || lookup.contains(dummyEntry(word+"_v__")) || lookup.contains(dummyEntry(word+"_n__"))
                            || lookup.contains(dummyEntry(word+"_a__")) || lookup.contains(dummyEntry(word+"_r__")))
         if (allowPrintWhenTiming) {
            if (recip && kw.startsWith("q")) {
               outp.println(s"$kw and $word are reciprocal synonyms" + tag)
               //println(s"$kw and $word are reciprocal synonyms"+tag)
            } else if (nonrecip && kw.startsWith("q")) {
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
   
   
