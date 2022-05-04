/** File "Heap.scala", for CSE250, Spring 2022
    Standard array implementation including heapSort
 */

import scala.reflect.ClassTag
import scala.collection.ArrayOps

class Heap[A: ClassTag](cap: Int, keyComp: (A,A) => Int) {
   private val heap: Array[A] = Array.ofDim[A](cap+1)
   var end = 1  //INV: 0 is swap, 1..cap is data
   
   private def swap(i:Int,j:Int): Unit = {     //REQ: 1 <= i,j <= n < end
      heap(0) = heap(i)
      heap(i) = heap(j)
      heap(j) = heap(0)
   }

   def fixDown(u: Int): Unit = {
      var st = u          //"stone" in text.  Note var needed in Scala for val params
      while (st < (end+1)/2) {
         val gc = if (keyComp(heap(2*st),heap(2*st+1)) > 0 || 2*st+1 >= end) 2*st else 2*st+1
         if (keyComp(heap(st), heap(gc)) < 0) {
            swap(st,gc)
            st = gc
         } else {
            return
         }
      }
   }
           
   def fixUp(u: Int): Unit = {
      var bubble = u        //note bubble > = 2 means parent exists and is bubble/2
      while (bubble >= 2 && keyComp(heap(bubble),heap(bubble/2)) > 0) {   
         swap(bubble,bubble/2)
         bubble = bubble / 2        //integer division
      }
   }

   def enqueue(item: A): Unit = {
      //if (end >= capacity) { ... }  //blah-blah...
      assert(end <= cap, "Attempt to overstuff the heap")
      heap(end) = item
      fixUp(end)
      end += 1      
   }
      
   def pop(): A = {        //dequeue in text and Scala generally
      val ret = heap(1)
      swap(1,end-1)        //sometimes it is useful to put max there
      end -= 1
      fixDown(1)
      return ret
   }

   def fromArray(arr: Array[A]): Unit = {  //A.k.a.: "makeHeap", "heapify"
      val sz = arr.size
      assert(sz <= cap, "Array too big")
      for (i <- 0 until sz) { heap(i) = arr(i) }
      heap(sz) = heap(0)
      end = sz+1
      for (u <- (end-1)/2 to 1 by -1) {  //INV: Sub-heaps rooted at v > u are valid
         fixDown(u)                      //now INV holds for u too
      }
   }

   def size = end-1

   def toList = heap.slice(1,end).toList

   def toSortedArray: Array[A] = {
      val sz = end-1
      while (end > 1) {
         pop()
      }
      return heap.slice(1,sz+1)
   }
}

object Heap extends App {
   val arr0 = Array(3,2,4,1,5)
   val h = new Heap[Int](15,(x:Int,y:Int) => x.compareTo(y))
   h.fromArray(arr0)
   println("Heaped: " + h.toList + " (end = "+h.end+")")
   println("Sorted: " + h.toSortedArray.toList)
   val arr1 = Array(4,7,4,9,5,2,1,11,17,23,2)
   h.fromArray(arr1)
   println("Heaped: " + h.toList)
   println("Sorted: " + h.toSortedArray.toList)
   val arr2 = Array(4,7,3,9,5,2,1,11,17,23,6,13)
   h.fromArray(arr2)
   println("Heaped: " + h.toList)
   println("Sorted: " + h.toSortedArray.toList)
}
