/** File "Hello3.scala" by KWR for CSE250, Fall 2022.  
    Adds option types to the fun.
 */
object Hello3 {                             //"object" not "class" is like "static" in C++/Java
    def main(args: Array[String]) = {      //templates are [...] not <...> as in C++/Java
        val s = new StringBuilder("Hello")                    //semicolon is optional
        var t = new StringBuilder("Would")
        s(0) = 'J'                         //arrays use (...) not [...]
        t(2) = 'r'
        println(s"$s $t!")                 //first s is special syntax, $ says next block is variable
        val os = Some(s)
        println(os)
        var ss = "Does this work?"
        //ss = os
        ss = s"$os"
        println(ss)
    }
}
