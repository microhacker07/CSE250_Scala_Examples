/** File "AIOLI.scala" by KWR for CSE250, Spring 2022.
    Requires having both ISR.scala and SortedSLL.scala compiled at same level.
    Array Into Ordered List with Iterator, using same keyComp as SortedSLL.

    CLASS INVS: Items are sorted non-descending according to keyComp in list part.
    Every existing index of the array holds a nonempty list, OR the array has
    just index 0 to an empty list (this necessitates a kludgey test for empty, alas)
 */
import scala.collection.mutable.ArrayBuffer

class AIOLI[A](keyComp: (A,A) => Int) extends ISR[A] { Outer =>     //uses SortedSLL.scala
   var theArray: ArrayBuffer[SortedSLL[A]] = new ArrayBuffer[SortedSLL[A]]
   private var _size = 0
   private var rskip = 1
   

   /** Iter adds three methods to standard Scala next() and hasNext for iterators.
       INV: Iterator is attached to the node prior to the item it designates
       INV: theArray(ind).item <= iat.item <= theArray(ind+1).item
            with iat "physically between" the two.
    */
   class Iter(var ind: Int, var iat: SortedSLL[A]#Iter) extends Iterator[A] {

      /** Special Scala syntax allows using just parens to return the data item.
       */
      def apply(): A = {
         assert(hasNext, "Attempt to fetch item past end in AIOLI\n" + Outer.diagnosticString)
         return iat()
      }

      def next(): A = {
         assert(hasNext, "Attempt to advance past end in AIOLI\n" + Outer.diagnosticString)
         //INV: implies ind is not last index of array, so ind+1 is valid
         if (iat.hasNext) {
            return iat.next()
         } else {
            ind += 1
            iat = theArray(ind).begin
            return iat()
         }
      }

      def hasNext: Boolean = (iat.hasNext || ind < theArray.length - 1)
      //Note: The CLASS INV that all indices have nonempty lists enables this code to be short

      def update(newItem: A) = {
         assert(hasNext, "Attempt to update item past end in AIOLI\n" + Outer.diagnosticString)
         iat.update(newItem)
      }

      def equals(other: Iter): Boolean = { ind == other.ind && iat.equals(other.iat) }
   }

   //Public Implementation of ISR Trait---sorting and keyComp don't change this.

   type I = Iter

   def begin: Iter = new Iter(0, theArray(0).begin)  //always exist, by second CLASS INV
   def end: Iter = new Iter(theArray.length-1, theArray(theArray.length-1).end)
      
   private def insertBefore(item: A, loc: Iter): Iter = {  //always keep same list
      _size += 1
      val thisList: SortedSLL[A] = theArray(loc.ind)
      //val liter = new thisList.Iter(loc.iat.preat.asInstanceOf[thisList.Node])
      //val itr = thisList.insertBefore(item, liter)
      val itr = thisList.insertBefore(item, loc.iat.asInstanceOf[thisList.Iter])
      //val itr = thisList.insert(item)
      return new Iter(loc.ind, itr)
   }
   /** REQuires (but doesn't test or enforce) that 
       keyComp(preat.item, item) <= 0 && keyComp(item,preat.next.item) <= 0.
       Safe usage is to call insert(item,findPlace(item)).
    */
   def insert(item: A, loc: Iter) = insertBefore(item, loc)
   def insert(item: A): Iter = {
      if (isEmpty) {
         theArray += new SortedSLL[A](keyComp)
         val itr = theArray(0).insert(item)
         _size += 1
         return new Iter(0, itr)
      } else {
         return insert(item,findPlace(item))
      }
   }

   /** Cannot violate the CLASS INV, so OK to use freely.
    */
   def remove(loc: Iter): A = {
      assert(loc.hasNext, "Attempt to remove past-end item")
      //control here means loc is on a real element
      _size -= 1
      val thisList = theArray(loc.ind)
      val tmp = thisList.remove(loc.iat.asInstanceOf[thisList.Iter])
      //val tmp = theArray(loc.ind).remove(loc())
      if (theArray(loc.ind).isEmpty) {
         theArray.remove(loc.ind)
      }
      return tmp
   }
   def remove(item: A): A = {
      val itr = find(item)
      assert(itr.hasNext, "Attempt to remove non-found item " + item + " in AIOLI\n" + diagnosticString)
      return remove(itr)
   }

   private def findPlace(item: A): Iter = {
      var left = begin
      var right = end
      if (right.iat.equals(left.iat) || keyComp(item, left()) < 0) {
         return left
      } //else INV: left.item <= item < right.item, with end.item == +infinity
      var ret = begin
      while (right.ind - left.ind >= 2) {
         val newInd = (right.ind + left.ind)/2   //integer division!
         val mid = new Iter(newInd, theArray(newInd).begin)  
         if (keyComp(item, mid()) < 0) {
            right = mid
         } else {
            left = mid
         }
      }
      if (keyComp(item, left()) == 0) { ret = left } else { ret = right }
      val iat = theArray(ret.ind).findPlace(item)
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
      var ret = ""
      for (i <- 0 until theArray.length) {  //so i+1 is safe
         ret += "" + i + "-->" + theArray(i).diagnosticString
      }
      ret
   }


}

   
