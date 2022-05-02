/** File "CloneWars.scala" by KWR for CSE250, Spring 2022
    Does not compile.
    Yet another Scala iterator limitation
 */

object CloneWars extends App {
   val myList = List(1,2,3,4,5)
   val liter1 = myList.iterator
   val liter2 = liter1.clone
}
