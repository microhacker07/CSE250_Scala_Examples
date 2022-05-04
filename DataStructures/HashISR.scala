/** File "HashISR.scala" by KWR for CSE250, Spring 2022.
    Requires having both ISR.scala and DLLISR.scala compiled at same level.
 */
import scala.collection.mutable.ArrayBuffer

import java.io.FileWriter   //makes it easy to append
import java.io.PrintWriter  //makes "print" and "println" available


/** REQ: numSlots > 0; itemMatch (passed into DLLISR!) should match whole items not just hash key
    Enforces own CLASS INV that 0 <= ind < numSlots by doing Math.floorMod(hash, numSlots) everywhere
    Note: as in Java, hashCode can be negative (!) and then % n would give a negative value!
 */
class HashISR[A](numSlots: Int, hashFun: A => Int, itemMatch: (A,A) => Boolean) extends ISR[A] { Outer =>
   var theTable: Array[DLLISR[A]] = Array.fill(numSlots)(new DLLISR[A](itemMatch))
   private var _size = 0

   /** Iter adds three methods to standard Scala next() and hasNext for iterators.
       INV1: Iter is attached to the list node it designates, not its "pre"
       INV2: Iter is never at the end position of a chain.
       INV3: End iterator has ind==numSlots; then we don't care about liat
    */
   class Iter(var ind: Int, var liat: DLLISR[A]#Iter) extends Iterator[A] {

      /** Special Scala syntax allows using just parens to return the data item.
       */
      def apply(): A = {
         assert(hasNext, "Attempt to fetch item past end in HashISR\n" + Outer.diagnosticString)
         return liat()   //note re-use of DLLISR#Iter.apply() here
      }

      //private[Outer] def adjustBin() = {      //not allowed, stupidly IMPHO
      private[HashISR] def adjustBin() = {      //like nextBin() in the text
         if (!liat.hasNext) {
            ind += 1
            while (ind < numSlots && theTable(ind).isEmpty) { ind += 1 }
            if (ind < numSlots) {
               liat = theTable(ind).begin
            }
         } //otherwise leaves at ind==numSlots end position, don't care about liat
      }

      def next(): A = {
         assert(hasNext, "Attempt to advance past end in HashISR\n" + Outer.diagnosticString)
         if (liat.hasNext) {
            val ret = liat.next() 
            adjustBin()
            return ret
         } else {          //should we forgive a violation of INV2 here??
            adjustBin()
            return next()  //not infinite loop, but could lead to assertion violation
         }
      }

      //def hasNext: Boolean = liat.hasNext   //absolutely relies on INV2 holding
      def hasNext: Boolean = { adjustBin(); liat.hasNext }  //"defensive driving"

      def update(newItem: A) = {
         assert(hasNext, "Attempt to update item past end in AIOLI\n" + Outer.diagnosticString)
         liat.update(newItem)
      }

      def equals(other: Iter): Boolean = { ind == other.ind && liat.equals(other.liat) }
      //override def clone = new Iter(ind, liat)
      override def clone = new Iter(ind, liat.clone)   //needed !!
   }

   //Public Implementation of ISR Trait---sorting and keyComp don't change this.

   type I = Iter

   def begin: Iter = {
      var itr = new Iter(0, theTable(0).begin) 
      itr.adjustBin()
      return itr
   }
   def end: Iter = new Iter(numSlots, theTable(numSlots-1).end)
      
   /** Insert before item in given linked list, even if loc.liat==end
    */
   private def insertBefore(item: A, loc: Iter): Iter = {  //always keep same list unless end
      _size += 1
      val thisList:DLLISR[A] = theTable(loc.ind)
      //val liter = new thisList.Iter(loc.liat.preat.asInstanceOf[thisList.Node])
      //val itr = thisList.insertBefore(item, liter)
      val itr = thisList.insert(item, loc.liat.asInstanceOf[thisList.Iter])
      //val itr = thisList.insert(item)
      return new Iter(loc.ind, itr)
   }
   /** Needed for compatibility with ISR trait; has a point if hash value is correct.
    */
   def insert(item: A, loc: Iter): Iter = {
      //if ((hashFun(item) % numSlots) == loc.ind) {
      if (Math.floorMod(hashFun(item), numSlots) == loc.ind) {
         return insertBefore(item, loc)
      } else {
         return insert(item)
      }
   }
   def insert(item: A): Iter = {
      //val ind = hashFun(item) % numSlots
      val ind = Math.floorMod(hashFun(item), numSlots)
      //assert(ind >= 0, "Item " + item + " has hash code " + hashFun(item))
      val thisList:DLLISR[A] = theTable(ind)
      val litr = thisList.insert(item, theTable(ind).begin.asInstanceOf[thisList.Iter])
      _size += 1
      return new Iter(ind, litr)
   }

   /** Cannot violate the CLASS INVs, so OK to use freely.
    */
   def remove(loc: Iter): A = {
      assert(loc.hasNext, "Attempt to remove past-end item")
      //control here means loc is on a real element
      _size -= 1
      val thisList = theTable(loc.ind)
      val tmp = thisList.remove(loc.liat.asInstanceOf[thisList.Iter])
      //val tmp = theTable(loc.ind).remove(loc())
      return tmp
   }
   def remove(item: A): A = {
      val itr = find(item)
      assert(itr.hasNext, "Attempt to remove non-found item " + item + " in AIOLI\n" + diagnosticString)
      return remove(itr)
   }

   def find(item: A): Iter = {
      //val ind = hashFun(item) % numSlots
      val ind =  Math.floorMod(hashFun(item), numSlots)
      val litr = theTable(ind).find(item)
      if (litr.hasNext) {
         return new Iter(ind, litr)
      } else {
         return end
      }
   }

   def size = _size

   //override def isEmpty = (_size <= 0)

   //override def ++=(other: ISR[A]): Unit   
   //Appending whole sequences  is now majorly dubious given the sortedness
   //invariant, so skip & ignore.

   def fromSortedArray(arr: Array[A]) = {
      theTable = Array.fill(numSlots)(new DLLISR[A](itemMatch))
      for (item <- arr) {
         insert(item)
      }
   }
      

   def diagnosticString = {
      var ret = ""
      for (i <- 0 until theTable.length) {  //so i+1 is safe
         ret += "" + i + "-->" + theTable(i).toList + "\n"
      }
      ret
   }


}

   
