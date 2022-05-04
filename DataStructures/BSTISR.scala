/** File "BSTISR.scala" by KWR for CSE250, Spring 2022.
    Requires having ISR.scala compiled at same level (since no package).
    Simple binary search tree using the ISR trait, paralleling text ch. 16, pp506-510
    but using a sentinel "end" node rather than null references.  This is done in the
    circularly linked fashion of CL&R(&S), with root.parent = end and end.parent = root.

    CLASS INV: Items are sorted non-descending by inorder according to keyComp in sequence
 */
class BSTISR[A](keyComp: (A,A) => Int) extends ISR[A] { Outer =>

   protected class Node(var item: A, var left: Node, var right: Node, var parent: Node) 
   private val endSentinel = new Node(null.asInstanceOf[A],null,null,null)
   private var root = new Node(null.asInstanceOf[A],endSentinel,endSentinel,endSentinel)
   endSentinel.left = root
   endSentinel.right = root
   endSentinel.parent = root
   private var _size = 0

   /** Helper method is inclusive---if x is a leaf or left elbow, it returns x.
    */
   private def leftmostBelow(x: Node): Node = if (x.left == endSentinel) x else leftmostBelow(x.left)

   /** Worded differently from "isRightChild" only to handle cases of root and endSentinel.
    */
   private def isNotLeftChild(u: Node) = (u != u.parent.left)

   /** The single most useful method to code first, even before the Iter class
       Defines a cycle that begins and ends with the sentinel.  Basic fact:
       If a node has a right subtree, the successor is the leftmost node in it,
       else the successor is an ancestor of the node.  Note also: if the node x is max,
       then the root is a right-child ancestor of x, but root counts as a left child of end.
       So we return root.parent, which in the CL&R tree is the end sentinel.  
       And given x = endSentinel, we have x.right == root, so we get the leftmost
       node below the root, which is the proper begin position.  So we get the desired cycle.
    */
   private def inorderSuccessor(x: Node): Node = {
      if (isEmpty) return endSentinel
      if (x.right != endSentinel) {
         return leftmostBelow(x.right)
      } //else
      var y = x
      while (isNotLeftChild(y)) {
         y = y.parent
      }
      return y.parent
   }


   /** Iter add three methods to standard Scala next() and hasNext for iterators.
       INV: Iterator is attached to the node *of* the item it designates.
    */
   class Iter(var at: Node) extends Iterator[A] {

      /** Special Scala syntax allows using just parens to return the data item.
       */
      def apply(): A = {
         assert(hasNext, "Attempt to fetch item past end")
         return at.item
      }

      def next(): A = {
         assert(hasNext, "Attempt to advance past end")
         val ret = at.item   //this needs a temporary
         at = Outer.inorderSuccessor(at)
         return ret
      }

      //def prev: Iter = {  //back up in a circle regardless
         //return new Iter(at.prev)
      //}

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

   def begin = new Iter(inorderSuccessor(endSentinel))  //Circular links convenient here

   def end: Iter = new Iter(endSentinel)   //And double links help here: O(1) time
      
   /** REQ: Node of loc is a leaf or elbow, and insertion will not violate sortedness
       Note that we make this private so that the REQ need only be enforced internally.
    */
   private def insert(item: A, loc: Iter, leftward: Boolean): Iter = {
      assert((leftward && loc.at.left == endSentinel) || (loc.at.right == endSentinel && !leftward), 
             "Attempt to munge an existing node while inserting below " + loc())
      if (leftward) {
         loc.at.left = new Node(item, endSentinel, endSentinel, loc.at)
         return new Iter(loc.at.left)
      } else {
         loc.at.right = new Node(item, endSentinel, endSentinel, loc.at)
         return new Iter(loc.at.right)
      }
   }

   /** Public version needed for consistency with ISR trait, though outmoded by insert(item)
    */
   def insert(item: A, loc: Iter): Iter = insert(item, loc, loc.at.left == endSentinel)
   

   def insert(item: A): Iter = {
      if (isEmpty) {
         assert(root.item == null, "Null not replaced in BST")
         root.item = item
         _size += 1
         return new Iter(root)
      } //else
      var parent = root  //we can't trust the parent link of our rover because it could be endSentinel
      var wasLeft = keyComp(item, root.item) < 0   //will make duplicate-key inserts "tend to append"
      //var wasLeft = keyComp(item, root.item) <= 0
      var rover = (if (wasLeft) root.left else root.right)
      while (rover != endSentinel) {
         parent = rover
         wasLeft = (keyComp(item, rover.item) < 0)
         //wasLeft = (keyComp(item, rover.item) <= 0)
         rover = (if (wasLeft) rover.left else rover.right)
      }
      _size += 1
      return insert(item, new Iter(parent), wasLeft)
   }
      
   /** Removing any Node is always legal by the sortedness invariant, but for non-leaf, non-elbow
       nodes requires swapping value with the inorderSuccessor before deleting its node.
       Note that the operation of searching for the node with item to remove is in other code.
       The code has to do extra work because it uses endSentinel not null links as in the text.
    */
   def remove(loc: Iter): A = {
      assert(loc.hasNext, "Attempt to remove past-end Node of unfound item")
      _size -= 1        //we will always delete something
      val tmp = loc()   //we will remove this item but not necessarily its node
      if (loc.at == root) {   //we treat it specially
         if (_size == 0) {
            root.item = null.asInstanceOf[A]
            return tmp
         } else if (root.left == endSentinel) {
            root = root.right
            root.parent = endSentinel
            endSentinel.parent = root
            endSentinel.left = root
            endSentinel.right = root
            return tmp
         } else if (root.right == endSentinel) {
            root = root.left
            root.parent = endSentinel
            endSentinel.parent = root
            endSentinel.left = root
            endSentinel.right = root
            return tmp
         } else {
            //we do nothing now because the root will not be moved.
         }
      }     //no else here: we want the fall-through if loc.at == root == a full internal node
            //the next two tests are redundant but handling needs loc.at.parent to be real node.
      if (loc.at.left == endSentinel) {   //its right subtree becomes parent's new subtree
         val parent = loc.at.parent
         if (loc.at.right != endSentinel) { loc.at.right.parent = parent }
         if (loc.at == parent.left) {
            parent.left = loc.at.right    //and this overwrites loc.at, so loc becomes invalid
         } else {
            parent.right = loc.at.right
         }
      } else if (loc.at.right == endSentinel) {   //its left subtree becomes parent's new subtree
         val parent = loc.at.parent
         if (loc.at.left != endSentinel) { loc.at.left.parent = parent }
         if (loc.at == parent.left) {
            parent.left = loc.at.left
         } else {
            parent.left = loc.at.left
         }
      } else {    //loc.at is a full binary node.  But this means its successor is not.
                  //So after swapping in that node's value, we can remove it with one more call.
         val u = inorderSuccessor(loc.at)  //which is "findVictim" in the text
         loc.at.item = u.item              //tree being mutable is very helpful here
         return remove(new Iter(u))        //this recursion is safe because it won't fall thru to else
      } 
      //control here means we were in one of the easier cases and loc.at is already spliced out.
      return tmp
   }
      
   /** Return full item removed, if "item" is a dummy.  If not found, return original dummy item.
    */
   def remove(item: A): A = {
      val itr = find(item)
      if (!itr.hasNext) { return item }   //and do nothing---this is how text treats "-="
      //else
      return remove(itr)
   }
 
   //private def findPlace(item: A, from: Iter = begin): Iter //NOT NEEDED HERE, by "not found" policy
   
   def find(item: A): Iter = {
      if (root.item == null) {
         return end
      } //else
      var rover = root
      while (rover != endSentinel) {
         val c = keyComp(item, rover.item)
         if (c == 0) {
            while (rover.left != endSentinel && keyComp(item,rover.left.item) == 0) {
               rover = rover.left
            }
            return new Iter(rover) 
         } else if (c < 0) {
            rover = rover.left
         } else {
            rover = rover.right
         }
      } //control here means not found
      return end
   }
            
   def size = _size

   //override def isEmpty = (_size <= 0)

   def clear(): Unit = {
      root = new Node(null.asInstanceOf[A],endSentinel,endSentinel,endSentinel)
      endSentinel.left = root
      endSentinel.right = root
      endSentinel.parent = root
       _size = 0
   }

   def fromSortedArray(arr: Array[A]): Unit = {
      if (arr.length == 0 ) {
         clear()
      } else {
         root = fromSortedArray(arr, 0, arr.length, endSentinel)
         endSentinel.parent = root   //omitting this causes subtle bug!
         _size = arr.size
      }
   }
   private def fromSortedArray(arr: Array[A], left: Int, right: Int, parent: Node): Node = {
      val span = right - left
      if (span == 0) {
         println("Not supposed to get left = right = " + left)
         return endSentinel
      } else if (span == 1) {
         return new Node(arr(left), endSentinel, endSentinel, parent)
      } else if (span == 2) {
         val v = new Node(arr(left), endSentinel, endSentinel, parent)
         val w = new Node(arr(left+1), endSentinel, endSentinel, v)
         v.right = w
         return v
      } else {
         val mid = (left + right)/2
         val v = new Node(arr(mid), endSentinel, endSentinel, parent)
         v.left = fromSortedArray(arr, left, mid, v)
         v.right = fromSortedArray(arr, mid+1, right, v)
         return v
      }
   }


   private val offset = 3
   def diagnosticString = "\n" + sidewaysString(root, 0)

   def sidewaysString(u: Node, indent: Int): String = {
      if (u == endSentinel || (u == root && isEmpty)) { return "" }
      if (u.left == endSentinel) {
         if (u.right == endSentinel) {
            return (" "*indent) + "() " + u.item
         } else {    //Right subtree is uppermost, so will rotate diagram 90 degreews right
            return sidewaysString(u.right, indent+offset) + "\n" + (" "*indent) + "() " + u.item
         }
      } else if (u.right == endSentinel) {   //now u.left must be non-null
         return (" "*indent) + "() " + u.item + "\n" + sidewaysString(u.left, indent+offset)
      } else {
         return (sidewaysString(u.right, indent+offset) + "\n"
                   + (" "*indent) + "() " + u.item + "\n"
                   + sidewaysString(u.left, indent+offset))
      }
   }

}

   
