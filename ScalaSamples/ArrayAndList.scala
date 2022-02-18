/** File "ArrayAndList.scala" by KWR for CSE250, Spring 2022.
    Illustrates similarities and differences between arrays and lists
 */
object ArrayAndList extends App {
   val cubesA = Array.tabulate(100)(i => i*i*i)
   var cubesL = List.tabulate(100)(i => i*i*i)
   println(s"Array item 17 cubed is ${cubesA(17)} and list item 18 cubed is ${cubesL(18)}")

   //Try to change the first element
   cubesA(0) = -1                      //"val" doesn't stop this
   //cubesA = Array(-1,0,1,8,27,64)    //it does stop this
   //cubesL(0) = -1   //List[Int] being immutable does stop this

   //Try to append 100 cubed.  The + goes toward the number, the : toward the array or list
   //cubesA :+= 100*100*100    //"val" does stop this, because it becomes the statement cubesA = cubesA :+ 100*100*100
   cubesL :+= 100*100*100      //var allows this.  Note: appending is slow with a List

   var cubesA2 = cubesA        //OK since cubesA2 is a fresh handle. But is this a fresh copy?
   //Change the first element of cubesA2 back to what it should be
   cubesA2(0) = 0
   println(s"Was this a fresh copy?  " +
      s"cubesA(0) = ${cubesA(0)}, cubesA2(0) = ${cubesA2(0)}; so ${cubesA(0) != cubesA2(0)}")
   
   cubesA2 = cubesA :+ 100*100*100    //Now we assign cubesA2 to be a modified copy
   //Now make the first element of cubesA2 bad again.  Does cubesA go bad with it?
   cubesA2(0) = -1
   println(s"Was this a fresh copy?  cubesA(0) = ${cubesA(0)}, cubesA2(0) = ${cubesA2(0)}; so ${cubesA(0) != cubesA2(0)}")

   //Try to prepend -1, which happens to be its own cube
   cubesA2 +:= -1              //OK since cubesA2 is var.  Becomes cubesA2 = -1 +: cubesA2
   var cubesL2 = -1 +: cubesL
   cubesL ::= -1               //The "cons" way of prending to a list.  Also OK: cubesL2 +:= -1  Efficient.
   if (cubesL == cubesL2) { println("The lists are equal") }

   //Alas, the indices don't shift:
   println(s"Now array item 17 cubed is ${cubesA(17)} and list item 18 cubed is ${cubesL(18)}")

   /** We can recursively match on lists.  Note return type must be specified as Double to use pow function
    */
   def cubeRootsL(xs: List[Int]): List[Double] = xs match {
      case Nil => Nil
      case x::xrest => scala.math.pow(x,1.0/3) :: cubeRootsL(xrest)
   }
   val roots = cubeRootsL(cubesL)

   println(s"Cube roots from indexes 10 to 19: ${roots.slice(10,20)}")

   /*--------------Can't do this with arrays---------------------
   def cubeRootsA(xs: Array[Int]): Array[Double] = xs match {
      case Nil => Nil
      case x::xrest => scala.math.pow(x,1.0/3) :: cubeRootsA(xrest)
   }
   *///----------------------------------------------------------

   /** But we can use for and yield to do this
    */
   def cubeRootsA(xs: Array[Int]): Array[Double] =
      for { x <- xs } yield scala.math.pow(x,1.0/3)

   val rootsA = cubeRootsA(cubesA)
   //println(s"Cube roots from indexes 10 to 19: ${rootsA.slice(10,20)}")
   println(s"Cube roots from indexes 10 to 19: ${rootsA.slice(10,20).toList}")

}
