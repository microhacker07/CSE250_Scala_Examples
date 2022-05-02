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
       INV: Iter is attached to the node it designates
       INV: Iter is never at the end position of a chain.
       INV: End iterator has ind==numSlots, whereupon iat is "rogue"
    */
   class Iter(var ind: Int, var iat: DLLISR[A]#Iter) extends Iterator[A] {

      /** Special Scala syntax allows using just parens to return the data item.
       */
      def apply(): A = {
         assert(hasNext, "Attempt to fetch item past end in HashISR\n" + Outer.diagnosticString)
         return iat()   //note re-use of ISR here
      }

      //private[Outer] def adjustBin() = {    
      private[HashISR] def adjustBin() = {      //like nextBin() in the text
      //def adjustBin() = {      //like nextBin() in the text
         if (!iat.hasNext) {
            ind += 1
            while (ind < numSlots && theTable(ind).isEmpty) { ind += 1 }
            if (ind < numSlots) {
               iat = theTable(ind).begin
            }
         } //can leave only at ind==numSlots end position
      }

      def next(): A = {
         assert(hasNext, "Attempt to advance past end in HashISR\n" + Outer.diagnosticString)
         //INV: implies ind is not last index of array, so ind+1 is valid
         if (iat.hasNext) {
            val ret = iat.next() 
            adjustBin()
            return ret
         } else {    //we forgive leaving iat at the end of a chain
            adjustBin()
            return iat()
         }
      }

      def hasNext: Boolean = (iat.hasNext || ind < theTable.length)
      //Note: The CLASS INV that all indices have nonempty lists enables this code to be short

      def update(newItem: A) = {
         assert(hasNext, "Attempt to update item past end in AIOLI\n" + Outer.diagnosticString)
         iat.update(newItem)
      }

      def equals(other: Iter): Boolean = { ind == other.ind && iat.equals(other.iat) }
      override def clone = new Iter(ind, iat)
   }

   //Public Implementation of ISR Trait---sorting and keyComp don't change this.

   type I = Iter

   def begin: Iter = {
      var itr = new Iter(0, theTable(0).begin) 
      itr.adjustBin()
      return itr
   }
   def end: Iter = new Iter(numSlots, theTable(numSlots-1).end)
      
   /** Insert before item in given linked list, even if loc.iat==end
    */
   private def insertBefore(item: A, loc: Iter): Iter = {  //always keep same list unless end
      _size += 1
      val thisList:DLLISR[A] = theTable(loc.ind)
      //val liter = new thisList.Iter(loc.iat.preat.asInstanceOf[thisList.Node])
      //val itr = thisList.insertBefore(item, liter)
      val itr = thisList.insert(item, loc.iat.asInstanceOf[thisList.Iter])
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
      val tmp = thisList.remove(loc.iat.asInstanceOf[thisList.Iter])
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

   
