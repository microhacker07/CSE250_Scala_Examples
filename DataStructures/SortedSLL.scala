/** File "SortedSLL.scala" by KWR for CSE250, Spring 2022.
    Requires having ISR.scala compiled at same level (since no package).
    Singly linked list with roll-your-own Iter[ator] inner class and
    with a passed-in key-comparator function keyComp.
    Parallels the way section 13.1 uses sorted linked lists.
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

    SCALA-SPECIFIC ELEMENTS: 
    (1) The ad-hoc syntax "Outer =>" immediately after the class brace allows referring to the
        outer *object* from an inner class.  It is most needed when a generic parameter like
        "[A]" is used---and saves typing the full name of the outer class with generic syntax.  
    (2) The reserved names "apply()" and "update()" in the Iter class correspond to the parens ()
        and the assignment = as in the rest of Scala.
    (3) Node must be "protected" in order for the Iter class to access it.
    (4) The "null.asInstanceOf" with the sentinel nodes saves the text's extra syntax on p421.
 */
class SortedSLL[A](keyComp: (A,A) => Int) extends ISR[A] { Outer =>     //changed from SLLISR.scala
   protected class Node(var item: A, var next: Node)
   private val endSentinel = new Node(null.asInstanceOf[A],null)
   private val headSentinel = new Node(null.asInstanceOf[A],endSentinel)
   private var _size = 0


   /** Iter adds three methods to standard Scala next() and hasNext for iterators.
       INV: Iterator is attached to the node prior to the item it designates
    */
   class Iter(var preat: Node) extends Iterator[A] {

      /** Special Scala syntax allows using just parens to return the data item.
       */
      def apply(): A = {
         assert(hasNext, "Attempt to fetch item past end in SortedSLL\n" + Outer.diagnosticString)
         return preat.next.item
      }

      def next(): A = {
         assert(hasNext, "Attempt to advance past end in SortedSLL\n" + Outer.diagnosticString)
         preat = preat.next
         return preat.item
      }

      def hasNext: Boolean = (preat != endSentinel && preat.next != endSentinel)

      def update(newItem: A) = {
         assert(hasNext, "Attempt to update item past end in SortedSLL\n" + Outer.diagnosticString)
         preat.next.item = newItem
      }

      def equals(other: Iter): Boolean = { preat == other.preat }
   }

   //Public Implementation of ISR Trait---sorting and keyComp don't change this.

   type I = Iter

   def begin = new Iter(headSentinel)  //is "at" the first item
   def end: Iter = {  //rather than maintain "penult" field, spend O(n) time in SLL
      var itr = begin
      while (itr.hasNext) itr.next()
      return itr
   }
      
   def insertBefore(item: A, loc: Iter): Iter = {
      loc.preat.next = new Node(item, loc.preat.next)
      _size += 1
      return loc
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
      val tmp = loc.preat.next.item
      loc.preat.next = loc.preat.next.next
      _size -= 1
      return tmp
   }
   def remove(item: A): A = {
      val itr = find(item)
      assert(itr.hasNext, "Attempt to remove non-found item " + item + " in SLL\n" + diagnosticString)
      return remove(itr)
   }

   def findPlace(item: A, from: Iter = begin): Iter = {
      var itr = from
      while (itr.hasNext && keyComp(item, itr()) > 0) {  
         itr.next()
      }
      //println("From " + item + ", SLL found " + (if (itr.hasNext) itr() else "end"))
      return itr
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
      var itr = begin
      var ret = "head sentinel: " + headSentinel + "\n"
      while (itr.hasNext) {
         ret += "" + itr.preat.next + ": " + itr.next() + "\n"
      }
      ret += "end sentinel: " + endSentinel + "\n"
      ret
   }

}

   
