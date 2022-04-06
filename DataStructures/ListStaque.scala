/** File "ListStaque.scala", by KWR for CSE250, Spring 2022
    Stack and Queue ADTs with linked-list implementations
    Bridges from text's sections 7.5-7.6 to Chapter 12 (12.7 vis-a-vis 12.4).
    Implementations use private method names for better clarity
 */

import scala.reflect.ClassTag  //when implementations need to lay out A in memory

trait Stack[A] {
   def peek: A
   def pop(): A
   def push(item: A): Unit
   def isEmpty: Boolean
   def size: Int   //do we promise O(1) time for this too?
}

trait Queue[A] {
   def peek: A
   def pop(): A    //preferable to "dequeue" IMPHO
   def enqueue(item: A): Unit
   def isEmpty: Boolean
   def size: Int   //do we promise O(1) time for this too?
}

trait Staque[A] extends Stack[A] with Queue[A]



/** Singly Linked List class implementing a Stack.  Subset of section 12.4, per section 12.7.
    Postpones indexing, insertion, removal, and overwrite (update) until later.
    Does not need ClassTag, because we need not lay out A items in memory like with Array.
    Unlike with ArrayStack, actions occur at the front rather than rear.
    Further difference: we maintain the size in a field.
 */
class ListStack[A] extends Stack[A] {   

   /** Inner class.  Can see the generic type A.  One line of code is enough!
    */
   private class Node(var item: A, var next: Node)
   private var head: Node = null  //INV: On legal first item unless list is empty.
   private var _size = 0

   private def peekFront: A = { assert(!isEmpty, "Peek at empty stack."); return head.item }
   private def popFront(): A = {
      assert(!isEmpty, "Pop from empty stack.")
      val ret = head.item
      head = head.next   //Scala does not require manual deallocation like in C++
      _size -= 1         //extra line adds a smidgen to the principal constant of the "O(1)"
      return ret
   }
   private def pushFront(item: A) = { head = new Node(item,head); _size += 1 }

   //public section
   def peek = peekFront
   def pop() = popFront()
   def push(item: A) = pushFront(item)
   def isEmpty: Boolean = (_size == 0) // (head == null)
   def size = _size
   //this one requires transversal
   override def toString = {
      var ret: List[A] = Nil
      var rover = head
      while (rover != null) {
         ret ::= rover.item
         rover = rover.next
      }
      ret.reverse.toString
   }
      
}

/** May as well implement all 3 of the "Staque" operations.
    Still short of all the operations in the text's section 12.4, just ones in section 12.7.  
    Uses end-sentinel without creating a separate ENode class as on page 421---the
       null.asInstanceOf[A] special-feature since Scala 2.8 enables a shortcut.
    INV: head is real node, penult.next == end, UNLESS list is empty
    The "UNLESS" needs a kludgey test on every push, but modern CPUs punish less for it.
 */
class ListStaque[A] extends Stack[A] with Queue[A] {
   private class Node(var item: A, var next: Node)
   private val end = new Node(null.asInstanceOf[A], null)   //note special feature

   private var head = end         //INV: head.item is real data unless list is empty
   private var penult = end
   private var _size = 0

   private def peekFront: A = { assert(!isEmpty, "Peek at empty Staque."); return head.item }
   private def popFront(): A = {
      assert(!isEmpty, "Pop from empty Staque.")
      val ret = head.item
      head = head.next   //Scala does not require manual deallocation like in C++
      _size -= 1         //extra line adds a smidgen to the principal constant of the "(91)"
      return ret
   }
   private def pushFront(item: A) = { 
      if (isEmpty) {
         head = new Node(item, end)
         penult = head
      } else {
         head = new Node(item, head)  //leaves penult situated OK
      }
      _size += 1
   }
   private def pushRear(item: A) = {
      if (isEmpty) {   //same code as for pushFront in this case
         head = new Node(item, end)
         penult = head
      } else {
         penult.next = new Node(item, penult.next)  //need to update penult too
         penult = penult.next
      }
      _size += 1
   }

   //public section
   def peek = peekFront
   def pop() = popFront()
   def push(item: A) = pushFront(item)
   def enqueue(item: A) = pushRear(item)
   def isEmpty: Boolean = (_size == 0) // (head == null)
   def size = _size
   //this one requires transversal
   override def toString = {
      var ret: List[A] = Nil
      var rover = head
      while (rover != end) {   //NOTE CHANGE using sentinel to mark end
         ret ::= rover.item
         rover = rover.next
      }
      ret.reverse.toString
   }
}


object ListStaque extends App {
   println("\n")
   println("Test of ListStack.")
   val as = new ListStack[String]   //note: no "capacity" argument
   as.push("All")
   as.push("Together")
   as.push("Now")
   println(s"The Stack size is now ${as.size} with top item ${as.peek}")
   println("Are we about to blow our stack---?---no since no limit on capacity!")
   try {
      as.push("No Blow")
   } catch {
      case ex: java.lang.AssertionError => println("Hopefully no-op is OK.")
   }
   println("Whole stack now: " + as.toString + "\n\n")

   println("Test of ListQueue (as ListStaque).")
   val aq = new ListStaque[String]
   aq.enqueue("Breakfast")
   aq.enqueue("Lunch")
   aq.enqueue("Dinner")
   println(s"The Queue size is now ${aq.size} with front item ${aq.peek}")
   println("Are we about to gorge our queue with Dessert?")
   try {
      aq.enqueue("Dessert")
   } catch {
      case ex: java.lang.AssertionError => println("Hopefully no-op is OK.")
   }
   println("Whole Queue now: " + aq.toString)
   aq.pop()
   aq.pop()
   println("Whole Queue now: " + aq.toString + "; now can we have Dessert and Drinks?")
   aq.enqueue("Dessert")
   aq.enqueue("Drinks")
   println("Whole Queue now: " + aq.toString + "with extra Dessert!\n\n")

   println("Test of ListStaque with pushing at both ends.")
   val asq = new ListStaque[String]
   asq.enqueue("My")
   asq.enqueue("Candle")
   asq.enqueue("Burns")
   asq.push("At")
   asq.push("Both")
   asq.push("Ends")
   println("Whole Staque now: " + asq.toString + "\n\n")
}
