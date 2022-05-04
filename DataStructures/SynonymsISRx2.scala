/** File "SynonymsISRx2.scala", by KWR for CSE250, Spring 2022
    Test code while writing Prelim II: adds iteration to find "special" synonyms.
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

object SpeechParts extends Enumeration {
  //type SpeechPart = Value
  val Adjective, Noun, Verb, Misc, Unknown = Value
}

/** Assignment 4 class changed to have a part-of-speech field
    INV: category is one of "noun", "verb", "adjective", or "misc" (or "unknown")
 */
case class SynonymEntry(key: String, kind: SpeechParts.Value, synonyms: StringBox)



//Use ISR classes for *both* the individual synonym lists *and* the whole book 

class StringBox extends Cardbox[String]((x,y) => x.compareTo(y))
//class StringBox extends Cardbox[String](13, x=>x.hashCode, (x,y) => x==y)

class SynonymBox(var keyComp: (SynonymEntry,SynonymEntry) => Int) extends Cardbox[SynonymEntry](keyComp) 
//class SynonymBox extends Cardbox[SynonymEntry](keyComp = (x,y) => x.key.compareTo(y.key)) 
//class SynonymBox extends Cardbox[SynonymEntry](10007, x=>x.key.hashCode, (x,y) => (x.key==y.key && x.kind==y.kind))
//class SynonymBox extends Cardbox[SynonymEntry](1000, x=>x.key.hashCode, (x,y) => (x.key==y.key))



/** Principal lines begin "KEY: " and "SYN: ", the latter possibly
    followed by similar lines with no headword and terminated by a line
    beginning "ANT:" or with "=".  Assume words of those lines begin in
    column 5.  Reader uses state pattern to work one line at a time.
    SAME as in the Map-based answer key, except for 2 lines as noted.
 */
object SynonymReader {
   def readEntries: ArrayBuffer[SynonymEntry] = {
      val synFile = "Fallows1898fx.txt"
      val src = Source.fromFile(synFile)
      var synarray = new ArrayBuffer[SynonymEntry]()
   
      var key = ""
      var inSyn = false
      var accumeLine = ""
      var count = 0
      var kind = SpeechParts.Unknown
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
                  kind = SpeechParts.Noun
               } else if (line.contains("\\v.\\")) {
                  //println("Added verb from line " + line)
                  kind = SpeechParts.Verb
               } else if (line.contains("\\a.\\")) {
                  //println("Added adjective from line " + line)
                  kind = SpeechParts.Adjective
               } else if (line.contains("\\r.\\")) {
                  //println("Added adjective from line " + line)
                  kind = SpeechParts.Misc
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
               accumeLine = accumeLine.trim()
               if (accumeLine.endsWith(".")) { accumeLine = accumeLine.dropRight(1) }
               val syns = accumeLine.split(",\\s+")
               if (syns.size > 0) {
                  //val item = new SynonymEntry(key, Set[String]())
                  //val item = SynonymEntry(key, SortedSet[String]())
                  //val item = SynonymEntry(key, ListBuffer[String]())
                  //ONLY CHANGES are here.  Need "insert" not "+=" to keep sortedness.
                  val item = SynonymEntry(key, kind, new StringBox())
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
            kind = SpeechParts.Unknown
         } else if (inSyn) {
            accumeLine += " " + line
         } else {
            //do nothing
         }
      }
   
      return synarray
            
   }
}
            
            
object SynonymsISRx2 extends App {
   val outp = new PrintWriter(new FileWriter("output.txt",true));  //appends
   def keyComp2(x: SynonymEntry, y: SynonymEntry): Int = x.key.compareTo(y.key)

   var synarray = SynonymReader.readEntries
   println("Created " + synarray.length + " entries.")

   //synarray = scala.util.Random.shuffle(synarray)
   //println("Shuffled " + synarray.length + " entries.")


   //val lookup = SortedMap.empty[String,SortedSet[String]]   //A4 key choice
   //val lookup = SortedMap.empty[String,StringBox]           //halfway ISR use choice
   val lookup = new SynonymBox(keyComp2)                      //ISR for both items and their synonym lists
   //val lookup = new SynonymBox()

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

   //val h = new Heap[SynonymEntry](10000, (x,y) => x.key.compareTo(y.key))
   //h.fromArray(synarray.toArray)
   //lookup.fromSortedArray(h.toSortedArray)

   val tm2 = System.nanoTime()
   var elapsedTime = (tm2 - tm1)/ms
   println("")
   println("SynonymBox creation took time " + elapsedTime + " ms, from " + lookup.size + " different entries")
   println("Iterating finds " + lookup.toList.size + " entries")
   println("\n\n\n\n")


   /** Needed because "find" etc. take a whole item, not just its key.  
       But it can be a dummy item in fields that aren't used for keys.
       Note that "null.asInstanceOf[StringBox]" is OK even when StringBox is a case class.
    */
   //def dummyEntry(key: String) = new SynonymEntry(key, new StringBox())
   //def dummyEntry(key: String) = new SynonymEntry(key, null.asInstanceOf[StringBox])
   def dummyEntry(key: String) = new SynonymEntry(key, SpeechParts.Unknown, new StringBox())


   var rover = lookup.begin
   while (rover.hasNext) {
      var itr = lookup.find(rover())
      //while (itr.hasNext && keyComp(item, itr()) == 0 && (!(itr().equals(rover())))) { itr.next() }
      //if (itr.hasNext && keyComp(item, itr()) == 0) {
      while (itr.hasNext && lookup.keyComp(rover(), itr()) == 0 && (!(itr().equals(rover())))) { itr.next() }
      if (itr.hasNext && lookup.keyComp(rover(), itr()) == 0) {
         val item = itr.next()
         while (itr.hasNext && lookup.keyComp(item, itr()) == 0) {
            itr.next()
         }
         if (itr.hasNext) {
            //return (item.synonyms.contains(itr().key))
            if (item.synonyms.contains(itr().key)) {
               println("Special pair: " + item.key + " (" + item.kind + ") with " + itr().key)
            }
         }
      } //control here means item not found or item is last word in dictionary
      //return false
      
/*-------------------------Alternate answer with scout lookahead pointer--------------------------
      var scout: lookup.Iter = rover.clone   //new lookup.Iter(rover)   //rover.clone();
      while (scout.hasNext && scout().key == rover().key) {
         scout.next()
      }
      if (scout.hasNext) {
         //var synitr = rover().synonyms.begin
         //while (synitr.hasNext) {
            //if (rover().key <= synitr() && synitr() <= scout().key) {
         if (rover().synonyms.contains(scout().key)) {
               println("Special pair: " + rover().key + " (" + rover().kind + ") with " + scout().key)
            //}
            //synitr.next()
         }
      }
*///----------------------------------------------------------------------------------------------
      rover.next()
   }


   println("\nTiming the main run now..." + (if (allowPrintWhenTiming) "" else "no printing...") + "\n")
   
   val t1 = System.nanoTime()

   var keyitr = lookup.begin
   while (keyitr.hasNext) {
      val entry = keyitr.next()
      val key = entry.key           //TASK: For each synonym "word" of key, lookup word and see if key is
      val synSet = entry.synonyms   //one of word's own synonyms.  Because word might appear in different
      var synitr = synSet.begin     //noun,verb,adj., etc. forms, we have to try multiple keys.
      var recip = false
      var hasSyns = false
      while (synitr.hasNext) {
         val word = synitr.next()
         val wordAsItem = dummyEntry(word)
         var worditr = lookup.find(wordAsItem)
         //while (worditr.hasNext && keyComp(wordAsItem,worditr()) == 0) {
         while (worditr.hasNext && word.compareTo(worditr().key) == 0) {
            recip = recip || worditr().synonyms.contains(key)
            hasSyns = hasSyns || (worditr().synonyms.size > 0)
            worditr.next()
         }
         if (allowPrintWhenTiming) {
            if (recip && key.startsWith("q")) {
               outp.println(s"$key and $word are reciprocal synonyms (key = " + entry.kind + ")")
               //println(s"$key and $word are reciprocal synonyms"+tag)
            } else if ((!recip) && hasSyns && key.startsWith("q")) {
               outp.println(s"$key lists $word but $word has a list of synonyms without $key")
               //println(s"$key lists $word but $word has a list of synonyms without $key")
            } else if (key.startsWith("q")) {
               //println(s"$key lists $word but that is not a key")
            }
         }
         recip = false
         hasSyns = false
      }
   }
               
   val t2 = System.nanoTime()
   elapsedTime = (t2 - t1)/ms
   println(s"Elapsed time in milliseconds: $elapsedTime")
   outp.close()
}
   
   
