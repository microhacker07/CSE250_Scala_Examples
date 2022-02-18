/** File "Hello.scala" by KWR for CSE250, Fall 2022.  A values & references puzzle.
 */
object Hello {                             //"object" not "class" is like "static" in C++/Java
    def main(args: Array[String]) = {      //templates are [...] not <...> as in C++/Java
        val s = "Hello";                   //semicolon is optional
        var t = "Would"
        s(0) = 'J'                         //arrays use (...) not [...]
        t(2) = 'r'
        println(s + " " + t + "!")         //"Hello World!" or "Jello World"---or bomb?
    }
}
