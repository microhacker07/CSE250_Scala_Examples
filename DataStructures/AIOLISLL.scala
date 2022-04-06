/** File "AIOLISLL.scala" by KWR for CSE250, Spring 2022.
    Requires having both ISR.scala and SortedSLL.scala compiled at same level.
    Array Into Ordered List with Iterator, using same keyComp as SortedSLL.
    Alternate implementation to "AIOLI.scala", using one single sorted linked list
    and making the array's "fingers" mark off segments of the list.

    CLASS INVS: Items are sorted non-descending according to keyComp in list part.
    Array part has iterArray(0) = SLL.begin, iterArray.last = SLL.end, remaining
    iterators obey nondescending sort in order of indexes.

    SCALA-SPECIFIC ELEMENTS:
    (1) The "Outer =>" syntax right after the opening class brace, noted in SortedSLL.scala.
    (2) The linked list's own iterators have to be typed as "theList.Iter" in order to 
        signify that they belong to the SortedSLL[A] object.
    (3) In order to "stabilize" theList so the compiler can lay out memory for Iter,
        theList itself has to be "val" not "var".  (It does not have to be immutable.)
    (4) The fact that theList is "val" does not prevent mutating its Nodes directly.
        (It might prevent using the "++=" operation, but this code has no desire to do that anyway.)
    (5) ArrayBuffer is needed to insert and remove *array cells* (as opposed to data items).
        [This class does not yet add new array cells, except for the initial "prepend" lines,
        but has code to remove a cell in the body of remove(loc: Iter).]
 */
import scala.collection.mutable.ArrayBuffer

class AIOLI[A](keyComp: (A,A) => Int) extends ISR[A] { Outer =>     //uses SortedSLL.scala
   val theList: SortedSLL[A] = new SortedSLL[A](keyComp)
   var fingerArray: ArrayBuffer[theList.Iter] = new ArrayBuffer[theList.Iter]
   private var _size = 0
   private var rskip = 1
   fingerArray.prepend(theList.end)
   fingerArray.prepend(theList.begin)


   /** Iter adds three methods to standard Scala next() and hasNext for iterators.
       INV: Iterator is attached to the node prior to the item it designates
       INV: fingerArray(ind).item <= iat.item <= fingerArray(ind+1).item
            with iat "physically between" the two.
    */
   class Iter(var ind: Int, var iat: theList.Iter) extends Iterator[A] {

      /** Special Scala syntax allows using just parens to return the data item.
       */
      def apply(): A = {
         assert(hasNext, "Attempt to fetch item past end in AIOLI\n" + Outer.diagnosticString)
         return iat()
      }

      def next(): A = {
         assert(hasNext, "Attempt to advance past end in AIOLI\n" + Outer.diagnosticString)
         //INV: implies ind is not last index of array, so ind+1 is valid
         val tmp = iat.next()
         if (iat.equals(fingerArray(ind+1))) { ind += 1 }
         return tmp
      }

      def hasNext: Boolean = iat.hasNext  //(!(iat.equals(theList.end)))

      def update(newItem: A) = {
         assert(hasNext, "Attempt to update item past end in AIOLI\n" + Outer.diagnosticString)
         iat.update(newItem)
      }

      def equals(other: Iter): Boolean = { ind == other.ind && iat.equals(other.iat) }
   }


   //Public Implementation of ISR Trait---sorting and keyComp don't change this.

   type I = Iter

   def begin = new Iter(0, theList.begin)  //is "at" the first item
   def end: Iter = new Iter(fingerArray.length-1, theList.end)
      

   /** Insertion and removal both need adjusting existing fingers into the list.
       This necessitates special cases for empty and begin- and end- cases.
       This method does not modify the array.
    */
   private def insertBefore(item: A, loc: Iter): Iter = {
      val liat = theList.insertBefore(item, loc.iat)
      if (isEmpty) {
         fingerArray(0) = liat
         val lastind = fingerArray.length-1
         fingerArray(lastind) = theList.end
         _size += 1
         return new Iter(0, liat)
      } //else
      if (liat.equals(theList.begin)) {
         fingerArray(0) = liat
      } 
      val liat2 = new theList.Iter(liat.preat.next)
      if (!liat2.hasNext) {      //need to adjust end.
         fingerArray(fingerArray.length-1) = liat2
      }
      _size += 1
      return new Iter(loc.ind, liat)
   }

   /** REQuires (but doesn't test or enforce) that 
       keyComp(preat.item, item) <= 0 && keyComp(item,preat.next.item) <= 0.
       Safe usage is to call insert(item,findPlace(item)).
    */
   def insert(item: A, loc: Iter) = insertBefore(item, loc)
   def insert(item: A): Iter = insert(item,findPlace(item))


   /** Removal cannot violate the CLASS INV, so this code is OK to use freely.
       Needs to shrink the array when the removed item was last in its segment,
       in order to preserve the INV that all fingers go to different nodes.
    */
   def remove(loc: Iter): A = {
      assert(loc.hasNext, "Attempt to remove past-end item")
      //control here means loc.ind+1 is safe in the array
      if (loc.iat.preat.next == fingerArray(loc.ind+1).preat) {
         fingerArray(loc.ind+1) = new theList.Iter(loc.iat.preat)
         if (fingerArray(loc.ind+1).equals(fingerArray(loc.ind)) && _size > 1) {
            fingerArray.remove(loc.ind+1)
         }
      }
      _size -= 1
      return theList.remove(loc.iat)
   }

   def remove(item: A): A = {
      val itr = find(item)
      assert(itr.hasNext, "Attempt to remove non-found item " + item + " in AIOLI\n" + diagnosticString)
      return remove(itr)
   }


   /** Binary search routine.  First finds index for start of segment in which item could go, then
       finishes off with simple linear search within that segment.  If item is not present, then
       it returns an iterator to an element before which the item could be inserted.
       INV: left.item is a real item and is <= item, and either item < right.item or right == end.
    */
   private def findPlace(item: A): Iter = {
      var left = begin
      var right = end
      if (right.iat.equals(left.iat) || keyComp(item, left()) < 0) {
         return left
      } //else INV: left.item <= item < right.item, with end.item == +infinity
      var ret = begin
      while (right.ind - left.ind >= 2) {
         val newInd = (right.ind + left.ind)/2   //integer division!
         val mid = new Iter(newInd, fingerArray(newInd))  
         if (keyComp(item, mid()) < 0) {
            right = mid
         } else {
            left = mid
         }
      }
      if (keyComp(item, left()) == 0) { ret = left } else { ret = right }
      val iat = theList.findPlace(item, ret.iat)
      val itrnew = new Iter(ret.ind, iat)
      println("From " + item + ", AIOLI found " + (if (itrnew.hasNext) itrnew() else "end"))
      return itrnew
   }

   def find(item: A): Iter = {
      val itr = findPlace(item)
      if (isEmpty || keyComp(item, itr()) == 0) return itr else return end
   }

   def size = _size

   //override def isEmpty = (_size <= 0)

   //override def ++=(other: ISR[A]): Unit   
   //Appending whole sequences  is now majorly dubious given the sortedness
   //invariant, so skip & ignore.

   def diagnosticString = {
      var ret = theList.diagnosticString
      for (i <- 0 until fingerArray.length - 1) {  //so i+1 is safe
         ret += "" + i + "-->"
         var liter = fingerArray(i)
         val liter2 = fingerArray(i+1)
         while (liter.hasNext && (!(liter.equals(liter2)))) {
            ret += "" + liter.next() + "\n"
         }
      }
      val eiter = fingerArray.last
      ret += "" + (fingerArray.length-1) + "-->" + eiter.preat + "\n"
      ret
   }


}

   
