/** File "DLLISR.scala" by KWR for CSE250, Spring 2022.
    Requires having ISR.scala compiled at same level (since no package).
    Doubly linked list with roll-your-own Iter[ator] inner class.
    Does NOT use O(n)-time indexing like the text does on pp418-419---instead it uses
    Iterators right away and so pro-actively avoids the "O(n^2) operations"
    danger mentioned at the bottom of page 415.  Iterator class Iter is 
    explicit, rather than instantiated anonymously via the (IMPHO cryptic) code
    on lines 57--65 on page 419 top.  Insertion and removal use iterator to mark
    the locations directly rather than use indexing to count up to it.
    Update and apply are likewise handled by the Iter class---so they too are
    O(1) time in themselves, though it may take O(n) time to *find* the place you want.

    Parallel to text section 12.5 but with changes and mostly simplifications.
    Circularly linked with one sentinel end node as in the text.
    Again, the "null.asInstanceOf[A]" feature avoids needing a default A _ object.
    CHANGED 3/17/22 to pass in a key-match function rather than use the Keyable trait.
 */
class DLLISR[A](keyMatch: (A,A) => Boolean) extends ISR[A] {

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

      def hasNext: Boolean = (at != endSentinel)

      def update(newItem: A) = {
         assert(hasNext, "Attempt to update item past end")
         at.item = newItem
      }

      def equals(other: Iter): Boolean = { at == other.at }
      override def clone = new Iter(at)
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

   def remove(loc: Iter): A = {
      assert(loc.hasNext, "Attempt to remove past-end item")
      loc.at.prev.next = loc.at.next
      loc.at.next.prev = loc.at.prev
      _size -= 1
      return loc.at.item
   }

   def find(item: A): Iter = {
      var itr = begin
      while (itr.hasNext && !(keyMatch(itr(),item))) {
         itr.next()
      }
      return itr
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

   
