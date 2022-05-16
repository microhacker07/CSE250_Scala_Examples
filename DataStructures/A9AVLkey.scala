/** File "A9key.scala", by KWR for CSE250, Spring 2022.
    Example test driver for BSTAVL.scala on Assignment 9, problem 1
    (but works in less exciting fashion for all other implementations of the ISR.scala API
    Requires ISR.scala, Cardbox.scala, and the Cardbox implementation compiled in the same folder.
    Besides implementing ISR, the implemented class also needs a "diagnosticString" method in
    order to run the current debug-print code (as of Mar. 27, 2022 release),

    SPECIAL SCALA ELEMENTS:
    (1) Although "keyComp" is defined as a method, it is passed in using lambda ("rocket") syntax
        as a "flat function"---this is a neat savings over turgid C++ pointer-to-method syntax.
    (2) Note as usual that box1 and box2 being "val" does not prevent their being changed...
 */

class StringBox extends Cardbox[String]((x,y) => x.compareTo(y))

object A9AVLkey extends App {
   val myavl = new StringBox()
   myavl.insert("draw")
   myavl.insert("the")
   myavl.insert("binary")
   myavl.insert("search")
   myavl.insert("tree")
   myavl.insert("that")
   println("\n\n----------------Tree after inserting \"that\"---------------------\n")
   println(myavl.diagnosticString)
   myavl.insert("results")
   myavl.insert("from")
   myavl.insert("inserting")
   println("\n\n----------------Tree after inserting \"inserting\"----------------\n")
   println(myavl.diagnosticString)
   myavl.insert("the")
   myavl.insert("words")
   myavl.insert("of")
   println("\n\n----------------Tree after inserting \"of\"-----------------------\n")
   println(myavl.diagnosticString)
   myavl.insert("this")
   myavl.insert("sentence")
   myavl.insert("in")
   myavl.insert("the")
   myavl.insert("order")
   myavl.insert("given")
   println("\n\n----------------Tree after inserting \"given\"-------------------\n")
   println(myavl.diagnosticString)
   myavl.insert("allowing")
   myavl.insert("duplicate")
   println("\n\n----------------Tree after inserting \"duplicate\"---------------\n")
   println(myavl.diagnosticString)
   myavl.insert("keys")
   println("\n\n----------------Tree after inserting \"keys\"--------------------\n")
   println(myavl.diagnosticString)
   myavl.remove("the")
   myavl.remove("the")
   myavl.remove("the")
   println("\n\n----------------Tree after removing \"the\" 3x-------------------\n")
   println(myavl.diagnosticString)
}
