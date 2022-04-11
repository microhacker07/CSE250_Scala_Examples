/** File "BALBOADLL.scala" by KWR for CSE250, Spring 2022.
    Requires having ISR.scala and SortedDLL.scala compiled at same level.
    Buffer As List Built Of Arrays.
    Uses native ArrayBuffer class accessed from doubly linked list.

    CLASS INVS: Items are sorted non-descending according to keyComp in list then arrays.
    The last node of the list holds an empty array, thus serving as the end sentinel,
    while every other node holds a nonempty array---so 0 is a valid index for the array.
 */
import scala.collection.mutable.ArrayBuffer

class BALBOADLL[A](keyComp: (A,A) => Int) extends ISR[A] { Outer =>     //uses SortedSLL.scala
   val theList: SortedDLL[ArrayBuffer[A]] = new SortedDLL[ArrayBuffer[A]]((x,y) => keyComp(x(0),y(0)))
   private var _size = 0
   private var rnumArrays = 1
   

   /** Iter adds three methods to standard Scala next() and hasNext for iterators.
       Uses integer indices lind for theList and aind for its constituent arrays.
       End iterator is (theList.length-1, 0), which designates the end sentinel node.
       Using integer indices for the ListBuffer part is *lame* because it means the operations
       are not O(1) time, but if the list part is short they are still fairly quick.
    */
   class Iter(var litr: theList.Iter, var aind: Int) extends Iterator[A] {

      /** Special Scala syntax allows using just parens to return the data item.
       */
      def apply(): A = {
         assert(hasNext, "Attempt to fetch item past end in AIOLI\n" + Outer.diagnosticString)
         //return litr.at.item(aind)
         assert(litr.hasNext && 0 <= aind && aind < litr().length,
                "Out of bounds index aind = " + aind + " in object\n" + Outer.diagnosticString)

         return litr()(aind)
      }

      def next(): A = {
         assert(hasNext, "Attempt to advance past end in AIOLI\n" + Outer.diagnosticString)
         //INV: Control here implies ind is not last index of array, so ind+1 is valid
         val tmp = this()
         if (aind < litr().length-1) { 
            aind += 1
         } else {
            aind = 0
            litr.next()
         }
         return tmp
      }

      def hasNext: Boolean = (litr.hasNext)
      //Note: The CLASS INV that all non-sentinel list indices have nonempty arrays enables this code to be short

      def update(newItem: A) = {
         assert(hasNext, "Attempt to update item past end in AIOLI\n" + Outer.diagnosticString)
         litr()(aind) = newItem
      }

      def equals(other: Iter): Boolean = { litr.equals(other.litr) && aind == other.aind }
   }

   //Public Implementation of ISR Trait---sorting and keyComp don't change this.

   type I = Iter

   def begin: Iter = new Iter(theList.begin, 0)  //always exist, by second CLASS INV
   def end: Iter = new Iter(theList.end, 0)
      
   private def insertBefore(item: A, loc: Iter): Iter = {  //always keep same list
      if (loc.equals(end)) {
         val prevlitr = loc.litr.prev
         prevlitr().append(item)
         _size += 1
         return new Iter(prevlitr, prevlitr().length-1)
      } //else
      loc.litr().insert(loc.aind, item)
      _size += 1
      return new Iter(loc.litr, loc.aind)
   }
   /** REQuires (but doesn't test or enforce) that 
       keyComp(preat.item, item) <= 0 && keyComp(item,preat.next.item) <= 0.
       Safe usage is to call insert(item,findPlace(item)).
    */
   def insert(item: A, loc: Iter) = insertBefore(item, loc)
   def insert(item: A): Iter = {
      if (isEmpty) {
         val litr = theList.insert(new ArrayBuffer(), theList.begin)
         litr().append(item)
         _size += 1
         return new Iter(litr,0)
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
      val tmp = loc.litr().remove(loc.aind)
      if (loc.litr().isEmpty) {
         theList.remove(loc.litr)
      }
      return tmp
   }
   def remove(item: A): A = {
      val itr = find(item)
      assert(itr.hasNext, "Attempt to remove non-found item " + item + " in BALBOADLL\n" + diagnosticString)
      return remove(itr)
   }

   private def findPlace(item: A): Iter = {
      var litr = theList.begin
      while (litr.hasNext && keyComp(item, litr.next()(0)) > 0) {
         //
      }
      //now litr is 1 or 2 places beyond where we wish to find.
      var litrprev = litr.prev
      if (!litrprev.equals(theList.begin)) { litrprev = litrprev.prev }
      var left = 0
      var right = litrprev().length
      if (right == left || keyComp(item, litrprev()(left)) < 0) {
         return new Iter(litrprev, left)
      } //else INV: left.item <= item < right.item, with end.item == +infinity
      while (right - left >= 2) {
         val mid = (right + left)/2   //integer division!
         if (keyComp(item, litrprev()(mid)) < 0) {
            right = mid
         } else {
            left = mid
         }
      }
      //INV: left() is a real item, since the array is nonempty
      if (keyComp(item, litrprev()(left)) == 0) {
         return new Iter(litrprev, left)
      } else if (right == litrprev().length) {
         return new Iter(litr, 0)
      } else { 
         return new Iter(litrprev, right)
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
      val litr = theList.begin
      var i = 0
      while (litr.hasNext) {  
         ret += "" + i + "-->" + litr.next().toList + "\n"
         i += 1
      }
      ret += "" + (i) + "-->" + "end sentinel"
      ret
   }


}

   
