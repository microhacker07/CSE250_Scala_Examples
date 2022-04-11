/** File ISR.scala, by KWR for CSE250, Spring 2022
    Information Storage and Retrieval interface 
    "Sandwiches" multiple possible implementations.
    CHANGED 3/17/22 to remove the "Keyable" trait and pass in
    a key-match function parameter instead.
 */

/** This trait not only has a generic type parameter A, it has a 
    concrete member type I that needs to be specified.  The type
    member I prevents having to do a gazillion "isInstance" typecasts
    with the iterators.  The whole thing shortcuts a bunch of Scala
    notions such as Traversable, Iterable, Sequence, Buffer...
 */
trait ISR[A] {
   type I <: Iterator[A]
   def begin: I                         //iterator to first element
   def end: I                           //iterator to last element: ! can need O(n) time.
   def insert(item: A, loc: I): I       //returns iterator to newly-inserted element
   def remove(loc: I): A                //returns removed element
   def find(item: A): I                 //returns iterator to found element
   def size: Int                        
   def isEmpty: Boolean = (size <= 0)

   //Some methods whose implementations come "for free"---need not be duplicated across code.
   //These "Generic Algorithms" could be coded by clients but are most convenient here.
   //This is a prime reason why unike Java interfaces, Scala traits may have real code.

   def +=(item: A): I = insert(item, end)

   /** Append other container to this one.  Do we cannibalize the other one?
       Either way, O(N) time, where other.size = N.  See note at bottom of SLLISR.scala
    */
   def ++=(other: ISR[A]): Unit = {
      //while (!other.isEmpty) {
         //insert(other.remove(other.begin), end)  //cannibalizes
      var itr = other.begin
      while (itr.hasNext) { 
         insert(itr.next(), end)   //breaks sortedness invariant
      }
   }

   /** This algorithm is "generic" meaning not just that it worksfor a generic type A
       but also because a client could have coded it for emself completely outside the class.
       No private fields or methods are used.  But having it as a method is convenient.
    */
   def toList: List[A] = {
      var ret = Nil: List[A]
      var itr = begin
      while (itr.hasNext) { ret ::= itr.next() }  //advances itr and returns value
      return ret.reverse
   }

   def contains(item: A): Boolean = find(item).hasNext
}

