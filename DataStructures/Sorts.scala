/** File "Sorts.scala" by KWR.  Improvement of code used for CSE250, Spring 2022.
    Compares MergeSort, QuickSort, and HeapSort on text files---with customizable settings.
    Gives millisecond times and counts of comparisons and copies (one swap = 3 copies).
    Typical usage with arguments <file> <tradePoint> <pivotChoice> <switches>:
   
    scala Sorts WarAndPeace.txt 16 medianRandom3 011000            

The final bitstring can also be in an int in 0..63; the above is 24 which is the default.  Index:
(randomize array)(use makeHeap)(partition for equals)(use selection sort)(make distinct)(pad lengths)

Strings are tagged with the index of their nth occurrence for n >= 2; the tags do not count
in comparisons unless the 5th bit "make distinct" is set.  The 6th option enables reading a
file of integers (whitespace or comma separated) and making the string comparison emulate integer <.
Output files "outputms.txt", "outputqs.txt", and "outpuths.txt" show how unstable quicksort and
heapsort are.  To test insertion sort and/or selection sort, choose tradePoint > n.

See also https://github.com/chrswt/algorithms-sedgewick/blob/master/notes/2.3-quicksort.md
 */

import io.StdIn._
import io.Source
import java.io.FileWriter   //makes it easy to append
import java.io.PrintWriter  //makes "print" and "println" available
import scala.reflect.ClassTag

import scala.collection.mutable.Map

            
case class SortLab[A: ClassTag](comp: (A,A) => Int) {

   var _tradePoint = 16
   var _useSelectionSort = false   //mergeSort only, since not in place
   var _pivotChoice = "medianRandom3"
   var _partitionForEquals = true
   var _useMakeHeap = true

   var msCopies = 0
   var msComps = 0
   var qsCopies = 0
   var qsComps = 0
   var hsCopies = 0
   var hsComps = 0
   var isCopies = 0
   var isComps = 0
   var ssCopies = 0
   var ssComps = 0

   def resetCounters() = {
      msCopies = 0
      msComps = 0
      qsCopies = 0
      qsComps = 0
      hsCopies = 0
      hsComps = 0
      isCopies = 0
      isComps = 0
      ssCopies = 0
      ssComps = 0
   }

   def mcomp(x:A,y:A) = { msComps += 1; comp(x,y) }
   def icomp(x:A,y:A) = { isComps += 1; comp(x,y) }
   def qcomp(x:A,y:A) = { qsComps += 1; comp(x,y) }
   def hcomp(x:A,y:A) = { hsComps += 1; comp(x,y) }
   def scomp(x:A,y:A) = { ssComps += 1; comp(x,y) }

   /** External insertion sort returns copy of array
    */
   def insertionSort(arr: Array[A]): Array[A] = {
      for (i <- 1 until arr.size) {
         val a = arr(i)
         isCopies += 1
         var j = i
         while (j > 0 && icomp(a, arr(j-1)) < 0) {
            arr(j) = arr(j-1)
            j -= 1;
            isCopies += 1
         }
         arr(j) = a
         isCopies += 1
      }
      return arr
   }


   /** SelectionSort on lists.  Option with MergeSort
    */
   def selectionSort(L: List[A]): List[A] = {
      var L1: List[A] = L
      var L2: List[A] = Nil
      var L3: List[A] = Nil
      while (!(L1.isEmpty)) {
         var itr = L1.iterator
         var min = itr.next()
         ssCopies += 1
         while (itr.hasNext) {
            val test = itr.next()
            ssCopies += 1
            if (scomp(test, min) < 0) {
               L2 ::= min
               min = test
               ssCopies += 2
            } else {
               L2 ::= test
               ssCopies += 1
            }
         }
         L3 ::= min
         ssCopies += 1
         L1 = L2   //do not need to reverse
         L2 = Nil
      }
      return L3.reverse
   }

//------------------------------ MergeSort -------------------------------------------

   def merge(L1: List[A], L2: List[A]): List[A] = {
      var itr1 = L1.iterator.buffered
      var itr2 = L2.iterator.buffered
      var L3: List[A] = Nil
      while (itr1.hasNext && itr2.hasNext) {
         if (mcomp(itr1.head, itr2.head) <= 0) {
            L3 ::= itr1.next()
         } else {
            L3 ::= itr2.next()
         }
         msCopies += 1
      }
      while (itr1.hasNext) {
         L3 ::= itr1.next()
         msCopies += 1
      }
      while (itr2.hasNext) {
         L3 ::= itr2.next()
         msCopies += 1
      }
      return L3.reverse
   }
         
   
   def halves(L: List[A]): (List[A], List[A]) = {
      return L.splitAt(L.length/2)
   }

   def mergeSort(L: List[A]): List[A] = {  //or same "B" trick
      if (L.length <= _tradePoint) { 
         return (if (_useSelectionSort) selectionSort(L) else insertionSort(L.toArray).toList)
      }
      val tup = halves(L)
      return merge(mergeSort(tup._1), mergeSort(tup._2))
   }


//----------------------------------QuickSort code, from text------------------------

   def median3(x:A, y:A, z:A, ix: Int, iy: Int, iz:Int): Int = 
      if (qcomp(x,y) < 0) 
         (if (qcomp(y,z) < 0) iy else (if (qcomp(x,z) < 0) iz else ix))
      else
         (if (qcomp(x,z) < 0) ix else (if (qcomp(y,z) > 0) iy else iz))


   /** Wrapper so that comp is only in top-level call and endpoints are below
       INV: In all ranges, "end" is exclusive, while "start" or "begin" is inclusive
       Whereas, both "low" and "high" can be inclusive
    */
   def quickSort(arr: Array[A], qsCap: Int = 10): Array[A] = {

      def pickPivotFirst(begin: Int, end: Int) = begin
      def pickPivotLast(begin: Int, end: Int) = end-1
      def pickPivotMiddle(begin: Int, end: Int) = (end + begin)/2  //integer division
      def pickPivotRandom(begin: Int, end: Int) = begin + util.Random.nextInt(end - begin)
      def pickPivotMedian3(begin: Int, end: Int): Int = {
         val mid = (end + begin)/2
         median3(arr(begin), arr(mid), arr(end-1), begin, mid, end-1)
      }
      def pickPivotMedianRandom3(begin: Int, end: Int): Int = {
         val i1 = begin + util.Random.nextInt(end - begin)
         val i2 = begin + util.Random.nextInt(end - begin)
         val i3 = begin + util.Random.nextInt(end - begin)
         median3(arr(i1), arr(i2), arr(i3), i1, i2, i3)
      }
      def pickPivotNinther(begin: Int, end: Int): Int = {
         val i1 = begin + util.Random.nextInt(end - begin)
         val i2 = begin + util.Random.nextInt(end - begin)
         val i3 = begin + util.Random.nextInt(end - begin)
         val i4 = begin + util.Random.nextInt(end - begin)
         val i5 = begin + util.Random.nextInt(end - begin)
         val i6 = begin + util.Random.nextInt(end - begin)
         val i7 = begin + util.Random.nextInt(end - begin)
         val i8 = begin + util.Random.nextInt(end - begin)
         val i9 = begin + util.Random.nextInt(end - begin)
         val ix = median3(arr(i1),arr(i2),arr(i3), i1, i2, i3)
         val iy = median3(arr(i4),arr(i5),arr(i6), i4, i5, i6)
         val iz = median3(arr(i7),arr(i8),arr(i9), i7, i8, i9)
         median3(arr(ix), arr(iy), arr(iz), ix, iy, iz)
      }


      def pickPivot = _pivotChoice match {
         case "first" => pickPivotFirst(_,_)
         case "last" => pickPivotLast(_,_)
         case "middle" => pickPivotMiddle(_,_)
         case "random" => pickPivotRandom(_,_)
         case "median3" => pickPivotMedian3(_,_)
         case "medianRandom3" => pickPivotMedianRandom3(_,_)
         case "ninther" => pickPivotNinther(_,_)
         case _ => pickPivotMedianRandom3(_,_)
      }
            

      /** Internal version works in-place, does not copy array
       */
      def inSort(start: Int, end: Int) = {
         for (i <- start+1 until end) {
            val a = arr(i)
            isCopies += 1
            var j = i
            while (j>start && icomp(a, arr(j-1)) < 0) {
               arr(j) = arr(j-1)
               j -= 1;
               //isComps += 1
               isCopies += 1
            }
            arr(j) = a
            isCopies += 1
         }
      }

      /** Partition routines return (low,high) where the recursion sorts
          arr[start..low) and arr[high..end).  Hence low is *exclusive downward*,
          which makes it OK to be the destination index of the pivot, or the
          inclusive bottom of the pivot's equal range.  And high is *inclusive*.
       */
      def partition2(start:Int, end:Int, pivotIndex: Int): (Int,Int) = {
         val p = arr(pivotIndex)
         arr(pivotIndex) = arr(start)
         arr(start) = p
         qsCopies += 3
         var (low, high) = (start+1, end-1)
         while (low <= high) {
            if (qcomp(arr(low), p) < 0) {
               low += 1
            } else {
               val tmp = arr(low)
               arr(low) = arr(high)
               arr(high) = tmp
               qsCopies += 3
               high -= 1
            }
         } //on exit, low = high+1, high is real and pivot goes there.
           //so for recursion, high is an exclusive upper bound, and
           //low is either an inclusive lower bound or is vacuous.
           //Lone defect is that either interval may have keys == p.
         arr(start) = arr(high)
         arr(high) = p
         qsCopies += 2
         // qsRecur(start, high)
         // qsRecur(low, end)
         return (high, low)
      }


      /** From https://yourbasic.org/golang/quicksort-optimizations/
          Tweaked to take the pivot's index as argument
       */
      def partition3(start:Int, end:Int, pivotIndex: Int): (Int,Int) = {
         val p = arr(pivotIndex)
         var low = start
         var high = end  //never dereferenced, only high-1
         var mid = low
	 while (mid < high) {
		// Invariant:
		//  - v[:low] < p
		//  - v[low:mid] = p
		//  - v[mid:high] are unknown
		//  - v[high:] > p
		//
		//         < p       = p        unknown       > p
		//     -----------------------------------------------
		// v: |          |          |a            |           |
		//     -----------------------------------------------
		//                ^          ^             ^
		//               low        mid           high
            val a = arr(mid)
            qsCopies += 1
            val c = qcomp(a, p)
            if (c < 0) {
               arr(mid) = arr(low)
               arr(low) = a
	       low += 1   //so low stays above the "< p" range
               mid += 1
               qsCopies += 2
            } else if (c == 0) {
               mid += 1
            } else {
               arr(mid) = arr(high-1)
               arr(high-1) = a
               high -= 1
               qsCopies += 2
            }
	 }
	 return (low,high)
      }

      def qsRecur(start:Int, end:Int): Unit = {    //end is exclusive in this code
         if (start < end - qsCap) {
            val pivotIndex = pickPivot(start, end)
            val p = arr(pivotIndex)
            var (low, high) = if (_partitionForEquals) partition3(start, end, pivotIndex)
                                 else partition2(start, end, pivotIndex)
            qsRecur(start, low)
            qsRecur(high, end)
         } else {
            inSort(start, end)
         }
      }
      qsRecur(0, arr.length)
      return arr
   }



   private var heap: Array[A] = Array.ofDim[A](0)
   var hend = 1  //INV: 0 is swap, 1..cap is data
   
   private def swap(i:Int,j:Int): Unit = {     //REQ: 1 <= i,j <= n < hend
      heap(0) = heap(i)
      heap(i) = heap(j)
      heap(j) = heap(0)
      hsCopies += 3
   }

   def fixDown(u: Int): Unit = {
      var st = u          //"stone" in text.  Note var needed in Scala for val params
      while (st < (hend+1)/2) {
         val gc = if (2*st+1 >= hend || hcomp(heap(2*st),heap(2*st+1)) > 0) 2*st else 2*st+1
         //if (2*st+1 >= hend) { hsComps -= 1 }
         if (hcomp(heap(st), heap(gc)) < 0) {
            swap(st,gc)
            st = gc
         } else {
            return
         }
         //hsComps += 2
      }
   }
           
   def fixUp(u: Int): Unit = {
      var bubble = u        //note bubble > = 2 means parent exists and is bubble/2
      while (bubble >= 2 && hcomp(heap(bubble),heap(bubble/2)) > 0) {   
         swap(bubble,bubble/2)
         bubble = bubble / 2        //integer division
         //hsComps += 1
      }
      //if (bubble >= 2) { hsComps += 1 }
   }

   def enqueue(item: A): Unit = {
      heap(hend) = item
      hsCopies += 1
      fixUp(hend)
      hend += 1      
   }
      
   def pop(): A = {        //dequeue in text and Scala generally
      val ret = heap(1)
      swap(1,hend-1)        //sometimes it is useful to put max there
      hend -= 1
      fixDown(1)
      return ret
   }

   def makeHeap(arr: Array[A]): Unit = {  //A.k.a.: "makeHeap", "heapify"
      val sz = arr.size
      heap = Array.ofDim[A](sz+1)
      for (i <- 0 until sz) { heap(i) = arr(i) }
      heap(sz) = heap(0)
      hend = sz+1
      for (u <- (hend-1)/2 to 1 by -1) {  //INV: Sub-heaps rooted at v > u are valid
         fixDown(u)                      //now INV holds for u too
      }
   }

   def heapSort(arr: Array[A]): Array[A] = {
      if (_useMakeHeap) {
         makeHeap(arr)
      } else {
         heap = Array.ofDim[A](arr.size + 1)
         hend = 1
         for (item <- arr) {
            enqueue(item)
         }
      }
      val sz = hend-1
      while (hend > 1) {
         pop()
      }
      return heap.slice(1,sz+1)
   }
}



object Sorts extends App {

   def tokenize2(line: String, carry: String = ""): List[String] = (line.headOption, carry) match {
      case (None, "") => Nil
      case (None, s) => s :: Nil
      case (Some(c), s) => if (c.isLetterOrDigit || c == '\'' || c == '-') {
         tokenize2(line.tail, s + c)
      } else if (c.isWhitespace) {
         if (s == "") tokenize2(line.tail, s) else s::tokenize2(line.tail, "")
      } else {
         if (s == "") {
            ("" + c) :: tokenize2(line.tail, "")
         } else s :: ("" + c) :: tokenize2(line.tail, "")
      }
   }

   if (args.length == 0) {
      println("Usage: scala Sorts <textfile> [tradePoint:Int=16] [pivotChoice:String='medianRandom3'] [bitstring-or-Int]")
      println("Pivot choices are: first, last, middle, random, median3, medianRandom3, ninther")
      println("Bits = (randomize array:0)(use makeHeap:1)(partition for equals:1)(use selection sort:0)(make distinct:0)(pad lengths:0)")
      println("Can use binary string of length 6 or integer in 0..63.  Use padLengths to give file of integers")
      sys.exit(0)
   }

   println("Bits = (randomize array)(use makeHeap)(partition for equals)(use selection sort)(make distinct)(pad lengths)")
   var randomizeArray = false
   var useMakeHeap = true
   var partitionForEquals = true
   var useSelectionSort = false
   var makeDistinct = false
   var padLengths = false
   if (args.length >= 4) {
      val str = args(3)
      val idx = str.toInt
      if (str.length == 6) {
         randomizeArray = (str(0) == '1')
         useMakeHeap = (str(1) == '1')
         partitionForEquals = (str(2) == '1')
         useSelectionSort = (str(3) == '1')
         makeDistinct = (str(4) == '1')
         padLengths = (str(5) == '1')
      } else {
         randomizeArray = (idx % 64 >= 32) 
         useMakeHeap = (idx % 32 >= 16)
         partitionForEquals = (idx % 16 >= 8)
         useSelectionSort = (idx % 8 >= 4)
         makeDistinct = (idx % 4 >= 2)
         padLengths = (idx % 2 == 1)
      }
   }
   val pivotChoice = if (args.length >= 3) args(2) else "medianRandom3"
   var tradeoffPoint: Int = if (args.length >= 2) args(1).toInt else 10
   val inputFile = if (args.length >= 1) args(0) else "words.txt"

   var maxWordLength = 0
   var strl: List[String] = Nil
   for (line <- Source.fromFile(inputFile).getLines()) {
      val lineList = tokenize2(line).filter(w => (w.head.isLetterOrDigit || w.head == '\'' || w.head == '-'))
      for (word <- lineList) {
         strl ::= word
         if (word.length > maxWordLength) { maxWordLength = word.length }
      }
   }
   var padded = ""
   if (padLengths) {
      padded = " padded"
      var altl: List[String] = Nil
      for (word <- strl) {
         var pad = "0" * (maxWordLength - word.length)
         altl ::= (pad+word)
      }
      strl = altl
   } else {
      strl = strl.reverse
   }
   val divider = (if (makeDistinct) "" else "_")
   var distinct = (if (makeDistinct) " made distinct" else "")
   val mymap = Map[String,Int]()
   var altl: List[String] = Nil
   for (word <- strl) {
      if (mymap.contains(word.toLowerCase)) {
         mymap(word.toLowerCase) += 1
         altl ::= (word + divider + mymap(word.toLowerCase).toString)
      } else {
         mymap(word.toLowerCase) = 1
         altl ::= word
      }
   }
   strl = altl.reverse
            
   println("Created long list of words")
      
   var stra = if (randomizeArray) scala.util.Random.shuffle(strl).toArray else strl.toArray
   
   var rnd = ""
   if (randomizeArray) {
      strl = stra.toList
      rnd = " randomized"
      println("Made randomly shuffled array")
   } else {
      println("Kept original list as array")
   }


   val baseSort = (if (useSelectionSort) "selection sort" else "insertion sort")
   val pfe = (if (partitionForEquals) "3-way" else "2-way")
   val mkh = (if (useMakeHeap) "makeHeap" else "repeated insertion")

   println(s"\nSorting ${stra.size}$rnd$padded items$distinct, tradepoint $tradeoffPoint to $baseSort")
   println(s"QuickSort pivot is $pivotChoice for $pfe partition; heapSort uses $mkh\n")

//----------------------------------Timing Tests------------------------------------

   val ms = 1000000.0

   def toUnder(str: String) = { val i = str.indexOf('_'); if (i >= 0) str.substring(0,i) else str }

   val ss = SortLab[String]((s1,s2) => toUnder(s1).toLowerCase.compareTo(toUnder(s2).toLowerCase))
   ss._tradePoint = tradeoffPoint
   ss._useMakeHeap = useMakeHeap
   ss._partitionForEquals = partitionForEquals
   ss._useSelectionSort = useSelectionSort

   val ts1 = System.nanoTime()

   val newArray = ss.mergeSort(strl)

   val ts2 = System.nanoTime()
   var elapsedTime = (ts2 - ts1)/ms
   println(s"\nMergeSort made ${ss.msComps} comparisons and ${ss.msCopies} copies")
   if (useSelectionSort) {
      println(s"plus   ${ss.ssComps} comparisons and ${ss.ssCopies} copies for selectionSort at bottom")
   } else {
      println(s"plus   ${ss.isComps} comparisons and ${ss.isCopies} copies for insertionSort at bottom")
   }
   var totalComps = ss.msComps + (if (useSelectionSort) ss.ssComps else ss.isComps)
   var totalCopies = ss.msCopies + (if (useSelectionSort) ss.ssCopies else ss.isCopies)
   println(s"In total: made $totalComps comparisons and $totalCopies copies")

   println("Time for mergeSort: " + elapsedTime + " milliseconds")

   ss.isComps = 0
   ss.isCopies = 0



   val tq1 = System.nanoTime()

   val newArray2 = ss.quickSort(stra, tradeoffPoint)

   val tq2 = System.nanoTime()
   elapsedTime = (tq2 - tq1)/ms
   println(s"\nQuickSort made ${ss.qsComps} comparisons and ${ss.qsCopies} copies")
   println(s"plus   ${ss.isComps} comparisons and ${ss.isCopies} copies for insertionSort at bottom")
   totalComps = ss.qsComps + ss.isComps
   totalCopies = ss.qsCopies + ss.isCopies
   println(s"In total: made $totalComps comparisons and $totalCopies copies")
   println("Time for quickSort: " + elapsedTime + " milliseconds")

   val th1 = System.nanoTime()

   val newArray3 = ss.heapSort(stra)

   val th2 = System.nanoTime()
   elapsedTime = (th2 - th1)/ms
   println(s"\nHeapSort made  ${ss.hsComps} comparisons and ${ss.hsCopies} copies")
   println("Time for heapSort: " + elapsedTime + " milliseconds")
   println("\n")

   val msout = new PrintWriter(new FileWriter("outputms.txt",false));  //appends
   for (word <- newArray) {
      msout.println(word)
   }
   msout.close()

   val qsout = new PrintWriter(new FileWriter("outputqs.txt",false));  //appends
   for (word <- newArray2) {
      qsout.println(word)
   }
   qsout.close()

   val hsout = new PrintWriter(new FileWriter("outpuths.txt",false));  //appends
   for (word <- newArray3) {
      hsout.println(word)
   }
   hsout.close()




}
