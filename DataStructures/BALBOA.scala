/** File "BALBOA.scala" by KWR for CSE250, Spring 2022.
    Requires having ISR.scala compiled at same level.
    Buffer As List Built Of Arrays.
    Uses native ListBuffer and ArrayBuffer classes.

    CLASS INVS: Items are sorted non-descending according to keyComp in list then arrays.
    The last node of the list holds an empty array, thus serving as the end sentinel,
    while every other node holds a nonempty array---so 0 is a valid index for the array.
 */
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

class BALBOA[A](keyComp: (A,A) => Int) extends ISR[A] { Outer =>     //uses SortedSLL.scala
   var theList: ListBuffer[ArrayBuffer[A]] = new ListBuffer[ArrayBuffer[A]]
   private var _size = 0
   private var rnumArrays = 1
   theList.append(new ArrayBuffer[A]())    //the sentinel
   

   /** Iter adds three methods to standard Scala next() and hasNext for iterators.
       Uses integer indices lind for theList and aind for its constituent arrays.
       End iterator is (theList.length-1, 0), which designates the end sentinel node.
       Using integer indices for the ListBuffer part is *lame* because it means the operations
       are not O(1) time, but if the list part is short they are still fairly quick.
    */
   class Iter(var lind: Int, var aind: Int) extends Iterator[A] {

      /** Special Scala syntax allows using just parens to return the data item.
       */
      def apply(): A = {
         assert(hasNext, "Attempt to fetch item past end in AIOLI\n" + Outer.diagnosticString)
         assert(0 <= lind && lind < theList.length && 0 <= aind && aind < theList(lind).length,
                "Out of bounds index " + lind + " and/or " + aind + " in object\n" + Outer.diagnosticString)
         return theList(lind)(aind)
      }

      def next(): A = {
         assert(hasNext, "Attempt to advance past end in AIOLI\n" + Outer.diagnosticString)
         //INV: Control here implies ind is not last index of array, so ind+1 is valid
         val tmp = this()
         if (aind < theList(lind).length-1) { 
            aind += 1
         } else {
            aind = 0
            lind += 1
         }
         return tmp
      }

      def hasNext: Boolean = (lind < theList.length-1)
      //Note: The CLASS INV that all non-sentinel list indices have nonempty arrays enables this code to be short

      def update(newItem: A) = {
         assert(hasNext, "Attempt to update item past end in AIOLI\n" + Outer.diagnosticString)
         theList(lind)(aind) = newItem
      }

      def equals(other: Iter): Boolean = { lind == other.lind && aind == other.aind }
   }

   //Public Implementation of ISR Trait---sorting and keyComp don't change this.

   type I = Iter

   def begin: Iter = new Iter(0, 0)  //always exist, by second CLASS INV
   def end: Iter = new Iter(theList.length-1, 0)
      
   private def insertBefore(item: A, loc: Iter): Iter = {  //always keep same list
      if (isEmpty && theList.length == 1) {
         theList.prepend(new ArrayBuffer[A]())
         theList(0).insert(0,item)
      } else if (loc.equals(end)) {
         theList(loc.lind-1).append(item)
          _size += 1
         return new Iter(loc.lind-1, theList(loc.lind-1).length-1)
      } //else
      theList(loc.lind).insert(loc.aind, item)
       _size += 1
      return new Iter(loc.lind, loc.aind)
   }
   /** REQuires (but doesn't test or enforce) that 
       keyComp(preat.item, item) <= 0 && keyComp(item,preat.next.item) <= 0.
       Safe usage is to call insert(item,findPlace(item)).
    */
   def insert(item: A, loc: Iter) = insertBefore(item, loc)
   def insert(item: A): Iter = {
      if (isEmpty) {
         theList.prepend(new ArrayBuffer())
         theList(0).append(item)
         _size += 1
         return new Iter(0,0)
      } else {
         val itr = findPlace(item)
         return insert(item,itr)
      }
   }

   /** Cannot violate the CLASS INV, so OK to use freely.
    */
   def remove(loc: Iter): A = {
      assert(loc.hasNext, "Attempt to remove past-end item")
      //control here means loc is on a real element
      _size -= 1
      val tmp = theList(loc.lind).remove(loc.aind)
      if (theList(loc.lind).isEmpty) {
         theList.remove(loc.lind)
      }
      return tmp
   }
   def remove(item: A): A = {
      val itr = find(item)
      assert(itr.hasNext, "Attempt to remove non-found item " + item + " in BALBOA\n" + diagnosticString)
      return remove(itr)
   }

   private def findPlace(item: A): Iter = {
      var lind = 0
      while (lind < theList.length-1 && keyComp(item, theList(lind)(0)) > 0) {
         lind += 1
      }
      if (lind > 0) { lind -= 1}  //the prev list may have other items with greater keys, so insert there.
      var left = 0
      var right = theList(lind).length
      if (right == left || keyComp(item, theList(lind)(left)) < 0) {
         return new Iter(lind, left)
      } //else INV: left.item <= item < right.item, with end.item == +infinity
      while (right - left >= 2) {
         val mid = (right + left)/2   //integer division!
         if (keyComp(item, theList(lind)(mid)) <= 0) {
            right = mid
         } else {
            left = mid
         }
      }
      //INV: left() is a real item, since the array is nonempty
      if (keyComp(item, theList(lind)(left)) == 0) {
         return new Iter(lind, left)
      } else if (right == theList(lind).length) {   //item is past end, move to next begin
         return new Iter(lind+1, 0)
      } else { 
         return new Iter(lind, right)
      }
   }

   def find(item: A): Iter = {
      val itr = findPlace(item)
      if (isEmpty || (itr.hasNext && keyComp(item, itr()) == 0)) return itr else return end
   }

   def size = _size

   //override def isEmpty = (_size <= 0)

   //override def ++=(other: ISR[A]): Unit   
   //Appending whole sequences  is now majorly dubious given the sortedness
   //invariant, so skip & ignore.

   def diagnosticString = {
      var ret = "Size = " + _size + "\n"
      for (i <- 0 until theList.length-1) {  //so i+1 is safe
         ret += "" + i + "-->" + theList(i).toList + "\n"
      }
      ret += "" + (theList.length-1) + "-->" + theList(theList.length-1).length + " end sentinel"
      ret
   }


}

   
