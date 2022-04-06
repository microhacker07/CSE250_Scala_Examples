/** File "SLLISR.scala" by KWR for CSE250, Spring 2022.
    Requires having ISR.scala compiled at same level (since no package).
    Singly linked list with roll-your-own Iter[ator] inner class.
    Does NOT use O(n)-time indexing like the text does on p409---instead it uses
    Iterators right away and so pro-actively avoids the "O(n^2) operations"
    danger mentioned at the bottom of page 415.  Iterator class Iter is 
    explicit, rather than instantiated anonymously via the (IMPHO cryptic) code
    on lines 70--78 on page 414.  Insertion and removal use iterator to mark
    the locations directly rather than use indexing to count up to it.
    Update and apply are likewise handled by the Iter class---so they too are
    O(1) time in themselves, though it may take O(n) time to *find* the place you want.
    Parallel to text section 12.4 (and 12.5 for doubly-linked list) but with 
    changes and mostly simplifications.  CHANGED 3/17/22 to pass in a function as a
    class argument, which is more flexible than using the previous "Keyable" trait.
 */
class SinglyLinkedList[A](keyMatch: (A,A) => Boolean) extends ISR[A] {
   protected class Node(var item: A, var next: Node)
   private val endSentinel = new Node(null.asInstanceOf[A],null)
   private val headSentinel = new Node(null.asInstanceOf[A],endSentinel)
   private var _size = 0


   /** Iter adds three methods to standard Scala next() and hasNext for iterators.
       INV: Iterator is attached to the node prior to the node with the item it designates
    */
   class Iter(var preat: Node) extends Iterator[A] {

      /** Special Scala syntax allows using just parens to return the data item.
       */
      def apply(): A = {
         assert(hasNext, "Attempt to fetch item past end")
         return preat.next.item
      }

      def next(): A = {
         assert(hasNext, "Attempt to advance past end")
         preat = preat.next
         return preat.item
      }

      def hasNext: Boolean = (preat.next != endSentinel)

      def update(newItem: A) = {
         assert(hasNext, "Attempt to update item past end")
         preat.next.item = newItem
      }

      def equals(other: Iter): Boolean = { preat == other.preat }
   }

   //Public Implementation of ISR Trait

   type I = Iter

   def begin = new Iter(headSentinel)  //is "at" the first item
   def end: Iter = {  //rather than maintain "penult" field, spend O(n) time in SLL
      var itr = begin
      while (itr.hasNext) itr.next()
      return itr
   }
      
   private def insertBefore(item: A, loc: Iter): Iter = {
      loc.preat.next = new Node(item, loc.preat.next)
      _size += 1
      return loc
   }
   def insert(item: A, loc: Iter): Iter = insertBefore(item, loc)

   def remove(loc: Iter): A = {
      assert(loc.hasNext, "Attempt to remove past-end item")
      val tmp = loc.preat.next.item
      loc.preat.next = loc.preat.next.next
      _size -= 1
      return tmp
   }

   def find(item: A): Iter = {
      var itr = begin
      while (itr.hasNext && !(keyMatch(itr(), item))) {  //see footnote
         itr.next()
      }
      return itr
   }

   def size = _size

   //override def isEmpty = (_size <= 0)

   //override def ++=(other: ISR[A]): Unit   Tempting to try to append in O(1) time by just
   //linking end.preat.next to other.begin.preat.next to splice the lists together.
   //But two roadblocks: (1) "other" would have to be SinglyLinkedList[A] not ISR[A], and
   //(2) In Scala, different from Java etc., inner classes like Node are /local to the object/
   //(buzzword: "path-dependent types").  This can be worked around with extra syntax 
   //including # replacing . in some places, but is not worth the trouble here IMPHO.  We may
   //use it in a simpler setting to connect Nodes of different Graphs together.

   //Footnote: The whole loop could just be
   //         while (itr.hasNext && !(keyMatch(itr.next(),item))) { }
   //with empty body.  Thus only the basic Java iterator ops next() and hasNext are needed,
   //and "find" could be coded inside the trait.  But we do NOT do this because other ADTs
   //will have internal implementations of find that are much more efficient than "roving".

}

   
