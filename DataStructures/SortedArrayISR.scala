/** File "SortedArrayISR.scala" by KWR for CSE250, Spring 2022.
    Requires having ISR.scala compiled at same level (since no package).
    ArrayBuffer with roll-your-own Iter[ator] inner class and
    with a passed-in key-comparator function keyComp.
    Parallels the wAy section 13.1 uses sorted arrays.
    Note that once you are finding stuff (such as priorities) via sorted keys,
    the idea of indexing is secondary---we use it to speed up sorted search.
    But the use of iterators is still primary.

    Note that the iterator-based insert can violate the following invariant
    by inserting an item at an iterator location that violates the sortedness.
    Whether to allow or rectify this will be a topic for discussion and essays.

    CLASS INV: Items are sorted non-descending according to keyComp in sequence

    Now "find" has a second dilemma: if item is not already stored, should we
    return the end iterator, or an iterator to the place where it could be inserted,
    i.e., to the place where we could firast tell it is not found?
    We make a *private* method findPlace for the latter behavior.
 */
import scala.collection.mutable.ArrayBuffer

class SortedArrayISR[A](keyComp: (A,A) => Int) extends ISR[A] {     //changed from SLLISR.scala
   var data = ArrayBuffer[A]()
   private var _size = 0


   /** Iter adds three methods to standard Scala next() and hasNext for iterators.
       INV: Iterator simply holds the index of the cell it denotes
    */
   class Iter(var at: Int) extends Iterator[A] {

      /** Special Scala syntax allows using just parens to return the data item.
       */
      def apply(): A = {
         assert(hasNext, "Attempt to fetch item past end")
         return data(at)
      }

      def next(): A = {
         assert(hasNext, "Attempt to advance past end")
         at += 1
         return data(at-1)
      }

      def hasNext: Boolean = (at < data.length)

      def update(newItem: A): Unit = {
         assert(hasNext, "Attempt to update item past end")
         data(at) = newItem
      }

      def equals(other: Iter): Boolean = { at == other.at } //BUGGY---see why?
   }

   //Public Implementation of ISR Trait---sorting and keyComp don't change this.

   type I = Iter

   def begin = new Iter(0)  //is "at" the first item
   def end: Iter = new Iter(data.length)  //O(1) time if length is O(1) time
      
   private def insertBefore(item: A, loc: Iter): Iter = {
      _size += 1
      data.insert(loc.at, item)
      return new Iter(loc.at)
   }
   
   /** REQuires (but doesn't test or enforce) that 
       keyComp(preat.item, item) <= 0 && keyComp(item,preat.next.item) <= 0.
       Safe usage is to call insert(item,findPlace(item)).
    */
   def insert(item: A, loc: Iter): Iter = insertBefore(item, loc)
   def insert(item: A): Iter = insert(item,findPlace(item))

   /** Cannot violate the CLASS INV, so OK to use freely.
    */
   def remove(loc: Iter): A = {
      assert(loc.hasNext, "Attempt to remove past-end item")
      val tmp = data(loc.at)
      data.remove(loc.at)
      _size -= 1
      return tmp
   }

   /** Uses binary search to find---this is the main benefit of using a sorted array
    */
   private def findPlace(item: A, from: Iter = begin): Iter = {
      var left = from
      var right = end  
      if (right.equals(left) || keyComp(item, left()) < 0) {
         return left
      } //else INV: left.item <= item < right.item, with end.item == +infinity
      while (right.at - left.at >= 2) {
         val mid = new Iter((right.at + left.at)/2)  //integer division!
         if (keyComp(item, mid()) < 0) {
            right = mid
         } else {
            left = mid
         }
      }
      if (keyComp(item, left()) == 0) return left else return right
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

}


