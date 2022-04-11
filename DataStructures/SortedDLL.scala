/** File "SortedSLL.scala" by KWR for CSE250, Spring 2022.
    Requires having ISR.scala compiled at same level (since no package).
    Singly linked list with roll-your-own Iter[ator] inner class and
    with a passed-in key-comparator function keyComp.
    Parallels the wAy section 13.1 uses sorted linked lists.
    Note that once you are finding stuff (such as priorities) via sorted keys,
    the idea of indexing is by-the-boards.  But Iterators are still useful.

    Insert and remove are the same as before---only find uses the sorting.
    Note that the iterator-based insert can violate the following invariant
    by inserting an item at an iterator location that violates the sortedness.
    Whether to allow or rectify this will be a topic for discussion and essays.

    CLASS INV: Items are sorted non-descending according to keyComp in sequence

    Now "find" has a second dilemma: if item is not already stored, should we
    return the end iterator, or an iterator to the place where it could be inserted,
    i.e., to the place where we could firast tell it is not found?
    We make a *private* method findPlace for the latter behavior.
 */
class SortedDLL[A](keyComp: (A,A) => Int) extends ISR[A] {

   protected class DNode(var item: A, var prev: DNode, var next: DNode)
   private val endSentinel = new DNode(null.asInstanceOf[A],null,null)
   endSentinel.prev = endSentinel
   endSentinel.next = endSentinel  //just like on text p418 top, can do this in Scala
   private var _size = 0


   /** Iter add three methods to standard Scala next() and hasNext for iterators.
       INV: Iterator is attached to the node *of* the item it designates.
       The fact of double links enables this conceptual simplification.
    */
   class Iter(var at: DNode) extends Iterator[A] {

      /** Special Scala syntax allows using just parens to return the data item.
       */
      def apply(): A = {
         assert(hasNext, "Attempt to fetch item past end")
         return at.item
      }

      def next(): A = {
         assert(hasNext, "Attempt to advance past end")
         val ret = at.item   //unlike with "preat", this needs a temporary
         at = at.next
         return ret
      }

      def prev: Iter = {  //back up in a circle regardless
         return new Iter(at.prev)
      }

      def hasNext: Boolean = (at != endSentinel)

      def update(newItem: A) = {
         assert(hasNext, "Attempt to update item past end")
         at.item = newItem
      }

      def equals(other: Iter): Boolean = { at == other.at }
   }

   //Public Implementation of ISR Trait

   type I = Iter

   def begin = new Iter(endSentinel.next)  //Circular links convenient here

   def end: Iter = new Iter(endSentinel)   //And double links help ehre: O(1) time
      
   private def insertBefore(item: A, loc: Iter): Iter = {
      loc.at.prev.next = new DNode(item, loc.at.prev, loc.at)
      loc.at.prev = loc.at.prev.next
      loc.at.prev.next = loc.at 
      _size += 1
      return new Iter(loc.at.prev)
   }
   def insert(item: A, loc: Iter): Iter = insertBefore(item, loc)
   def insert(item: A): Iter = insert(item, findPlace(item))

   def remove(loc: Iter): A = {
      assert(loc.hasNext, "Attempt to remove past-end item")
      loc.at.prev.next = loc.at.next
      loc.at.next.prev = loc.at.prev
      _size -= 1
      return loc.at.item
   }

   private def findPlace(item: A, from: Iter = begin): Iter = {
      var itr = from
      while (itr.hasNext && keyComp(item, itr()) > 0) {
         itr.next()
      }
      return itr
   }
   def find(item: A): Iter = {
      val itr = findPlace(item)
      if (isEmpty || (itr.hasNext && keyComp(item, itr()) == 0)) return itr else return end
   }

   def size = _size

   //override def isEmpty = (_size <= 0)

   def diagnosticString = {
      var itr = begin
      var ret = ""
      while (itr.hasNext) {
         ret += "" + itr.at + ": " + itr.next() + "\n"
      }
      ret += "end sentinel: " + endSentinel + "\n"
      ret
   }

}

   
