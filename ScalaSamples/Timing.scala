/** File "Timing.scala" by KWR for CSE250, Spring 2022.
    Illustrates use of scala System.nanoTime
 */
object Timing extends App {
   val ms = 1000000.0
   val t1 = System.nanoTime()
   //var arr = Array.tabulate(1000000)(i => i)
   var arr = Array.fill(1000000)(7)
   val t2 = System.nanoTime()
   //var lst = List.tabulate(1000000)(i => i)
   var lst = List.fill(1000000)(7)
   val t3 = System.nanoTime()
   println(s"\nCreation times (ms): array ${(t2 - t1)/ms}, list ${(t3 - t2)/ms}")

   println("\nJust to time prepending an element to each")
   val t4 = System.nanoTime()
   arr +:= -1
   val t5 = System.nanoTime()
   lst +:= -1
   val t6 = System.nanoTime()
   println(s"Prepend times (ms): array ${(t5 - t4)/ms}, list ${(t6 - t5)/ms}")

   println("\nHow about appending an element to each---")
   val t7 = System.nanoTime()
   arr :+= 1000000
   val t8 = System.nanoTime()
   lst :+= 1000000
   val t9 = System.nanoTime()
   println(s"Append times (ms): array ${(t8 - t7)/ms}, list ${(t9 - t8)/ms}")


   println("\nNow time to prepend onto a fresh array or list:")
   val t10 = System.nanoTime()
   var arr2 = -8 +: arr
   val t11 = System.nanoTime()
   var lst2 = -8 +: lst
   val t12 = System.nanoTime()
   println(s"Update times (ms): array ${(t11 - t10)/ms}, list ${(t12 - t11)/ms}")
}
