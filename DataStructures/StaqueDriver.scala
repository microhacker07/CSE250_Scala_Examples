/** File "StaqueDriver.scala", by KWR and TAs for CSE250, Spring 2022
    Illustration of a Stack and Queue ADT with array implementations
    Parallel to text, sections 7.5-7.6, but with some differences:
    (1) Array implementations use fixed size, *no amortization*.
        Thus operations are guaranteed O(1) time, but must beware overflow.
    (2) Exemplifies assertions, as on page 225 of Chapter 7, for peek and
        pop too.  But SKIPS section 7.7---detail on p228 is too low-level.
    (3) Implementations use private method names for better clarity
 */

import scala.reflect.ClassTag  //when array implementations need to lay out A in memory

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


/** "Classic" capacity-limited array implementation of a stack.
    Differs from text section 7.5 by not resizing the array.
    Max size is n. INV: 0 <= top <= n, top==n means array full.
    INV: top denotes place new item will go, not place of top item itself
    Data array being "val" means we cannot append or prepend to the array itself;
    instead the array "grows" virtually by changing the bounding value of "top".
    We could also make the array's own data immutable by using Vector instead.
 */
class ArrayStack[A: ClassTag](val capacity: Int) extends Stack[A] {
   private val data: Array[A] = new Array[A](capacity)
   private var top: Int = 0
   private def peekRear: A = { assert(!isEmpty, "Peek at empty stack."); data(top-1) }
   private def popRear(): A = { assert(!isEmpty, "Pop from empty stack."); top -= 1; return data(top) }
   private def pushRear(item: A): Unit = { assert(!isFull, "Stack Overflow!"); data(top) = item; top += 1 }
   private def isFull: Boolean = { top >= capacity }  //private since not in trait
   //public section
   def peek = peekRear
   def pop() = popRear() 
   def push(item: A) = pushRear(item)
   def isEmpty: Boolean = (top == 0)
   def size: Int = top
   //extra public overrides are AOK
   override def toString = data.slice(0,top).toSeq.toString
}

/** "Classic" capacity-limited circular array implementation of a queue.
    Differs from text section 7.6 by not resizing the array.
    Data creeps forward and wraps around as needed.  
    Unlike stack, cannot fill every cell, since could not tell apart from empty case
    So array's size n has to be 1 larger than client's requested capacity.
    INV: front is toward 0, 0 <= front,rear < n, so both always in range
    INV: front is real item unless rear == front; rear is space for next item
 */
class ArrayQueue[A: ClassTag](val capacity: Int) extends Queue[A] {
   protected val capp1 = capacity + 1    //= size n of array
   protected val data: Array[A] = new Array[A](capp1)
   protected var front: Int = 0   
   protected var rear: Int = 0     //INV: always denotes the place next element can go
   protected def peekFront: A = { assert(!isEmpty, "Peek at empty Queue"); data(front) }
   protected def popFront(): A = { 
      assert(!isEmpty, "Attempt to pop from empty Queue")
      val tmp = data(front);
      front = (front + 1) % capp1;
      return tmp 
   }
   protected def pushRear(item: A): Unit = { 
      assert(!isFull,"Attempt to add to full Queue")
      data(rear) = item;
      rear = (rear + 1) % capp1
   }
   protected def isFull = (front == (rear + 1) % capp1)
   //public section
   def peek = peekFront 
   def pop() = popFront() 
   def enqueue(item: A) = pushRear(item)
   def isEmpty: Boolean = (rear == front)
   def size: Int = (capp1 + rear - front) % capp1
   override def toString = if (front <= rear) {
      data.slice(front,rear).toSeq.toString
   } else {
      (data.slice(front,capp1) ++ data.slice(0,rear)).toSeq.toString
   }
}

/** Staque.  We *could* make a full-fledged Deque by implementing popRear() too.
    But nothing corresponding to that method is listed in our Staque trait.  
    Doing so would make clients dependent on a method in a particular implementation
    that is not committed to by alternate (improved) implementations.
 */
class ArrayStaque[A: ClassTag](override val capacity: Int) extends ArrayQueue[A](capacity) with Staque[A] {
   protected def pushFront(item: A) = {   //now interpreted as push of stack, in front not rear
      assert(!isFull,"Attempt to add to full Staque")
      front = (front + capp1 - 1) % capp1
      data(front) = item
   }
   //public implementation of trait method
   def push(item: A) = pushFront(item)   //note how the implementation of both push and pop
}                                        //is different from in the standalone Stack


object StaqueDriver extends App {
   println("\n")
   println("Test of ArrayStack.")
   val as = new ArrayStack[String](3)
   as.push("All")
   as.push("Together")
   as.push("Now")
   println(s"The Stack size is now ${as.size} with top item ${as.peek}")
   println("Are we about to blow our stack?")
   try {
      as.push("Blow")
   } catch {
      case ex: java.lang.AssertionError => println("Hopefully no-op is OK.")
   }
   println("Whole stack now: " + as.toString + "\n\n")

   println("Test of ArrayQueue.")
   val aq = new ArrayQueue[String](3)
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
   println("Whole Queue now: " + aq.toString + "\n\n")

   println("Test of ArrayStaque with pushing at both ends.")
   val asq = new ArrayStaque[String](6)
   asq.enqueue("My")
   asq.enqueue("Candle")
   asq.enqueue("Burns")
   asq.push("At")
   asq.push("Both")
   asq.push("Ends")
   println("Whole Staque now: " + asq.toString + "\n\n")
}



