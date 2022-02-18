/** File "Hello2.scala" by KWR for CSE250, Fall 2022.  A values & references puzzle.
    Also illustrates Scala 3 syntax not available yet in Scala 2
 */
object Hello {                             //"object" not "class" is like "static" in C++/Java
    def main(args: Array[String]) = {      //templates are [...] not <...> as in C++/Java
        val s = new StringBuilder("Hello")                    
        var t = new StringBuilder("Would")
        s(0) = 'J'                         //arrays use (...) not [...]
        t(2) = 'r'
        if (1 < 2) { println(s"$s $t!") } //"Hello World!" or "Jello World!"?
    }
}
