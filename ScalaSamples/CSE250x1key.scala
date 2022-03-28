/** File "CSE250x1key.scala", by KWR for CSE250, Spring 2022.
    Full key to Prelim I, with both comments and runnable code.
 */
import scala.math._

/** Problem (1) class definition
 */
class Polar(val radius: Double, var theta: Double) {  
   def realPart = radius*cos(theta)                             
}

object CSE250x1key extends App {

   val w = new Polar(1.0,0.0)
   var z = new Polar(1.0,Pi)
   var ell = List(w,z)
   val arr = Array(w,z)

   w.theta = Pi                 //(a) Yes, legal: w being val does not protect fields
   //z.radius = 3.0             //(b) No, illegal: radius is val in class
   z = new Polar(3.0,z.theta)   //(c) Yes, legal: z is var
   //z.theta = w                //(d) No, illegal: w is wrong type
   //ell(0) = new Polar(1.0,Pi) //(e) No, illegal: List is immutable
   ell ::= new Polar(1.0,Pi)    //(f) Yes, legal: ell is var
   ell(0).theta = Pi            //(g) Yes, legal: List makes entries immutable but not their fields
   //arr :+= w                  //(h) No, illegal: equivalent to arr = arr :+ w, and arr is val.

   var v = new Polar(1.0,0.0)
   z = v
   z.theta = Pi
   println("Now v.realPart = " + v.realPart)     
   //prints -1, which is cos(pi): As in Java, "z = v" is a reference copy (esp. with "var"),
   //so z.theta = Pi also makes v.theta = Pi

   //Contrast with example for the makeup exam, in which a true copy is made:
   var u = new Polar(v.radius,v.theta)
   u.theta = Pi/2.0
   println("Now v.realPart = " + v.realPart)


   /** Answers to Problem (2).  In 2(a), the point is that the split("\\s+")
       operation on a line takes time proportional to the length of the line.
       So if the lines have length n (or enough to say length r ~= n on average),
       and there are n such lines, the total time will be Theta(n^2) --- or you
       can say O(n^2) best possible.

       In 2(b), on the other hand, the time per line is presumed to be constant (*if* .length
       is implemented in constant time), so the total time is O(1) x n = O(n) best possible.

       Here are the answers to 2(c) and 2(d).  One thing to note is that a product over an
       empty domain gives the value 1 by default, not 0.  Answer C) is wrong anyway because
       it doesn't just give 0 on the empty list, it always zeroes the entire product.
    */
   def prod(lis: List[Int]): Int = lis match {
      //case Nil => 1; case x::rest => x + prod(rest) }    //error: gives 1+sum not product
      case Nil => 1; case x::rest => x*prod(rest) }        //correct
      //case Nil => 0; case x::rest => x*prod(rest) }      //error: zeroes the entire product
      //case Nil => x; case x::rest => prod(rest) }        //error: first x is out of scope
   
   def max(lis: List[Int], prevMax: Int = 0): Int = lis match {
      //A)
      //case Nil => 0         //No: always outputs 0.  Same issue with C)
      //case x::rest => if (x < prevMax) max(rest,prevMax) else max(rest,x) 
      //B)
      //case Nil => prevMax   //GOOD TOO; intent was  x   here and     prevMax  there
      //case x::rest => if (x < prevMax) max(rest,prevMax) else max(rest,x)
      //C)
      //case Nil => 0         //No
      //case x::rest => if (x > prevMax) max(rest,x) else max(rest,prevMax)
      //D)
      case Nil => prevMax     //Correct---as intended.
      case x::rest => if (x > prevMax) max(rest,x) else max(rest,prevMax)
   }

   //The teacherly point regarding B) was supposed to be that the following gives min not max:
   // case x::rest => if (x < prevMax) max(rest,x) else max(rest,prevMax)
   
   val ell2 = List(1,2,3,4,2)
   println("prod = " + prod(ell2) + " and max = " + max(ell2))
   

   def explode(s:String): List[Char] = s.headOption match {
      case None => Nil
      case Some(c) => c :: explode(s.tail)
   }

   val str = "MI5 sent James Bond 007 to -22.7 south latitude"

   /** Compact answer using match-case recursion.  The most common errors when using
       this style were forgetting to test acc == "" either in the base case (-3) or
       in the "else" branch (-3, but making both errors was only -5).
    */
   def extractInts1(str: String, acc: String = ""): List[String] = str.headOption match {
      case None => if (acc == "") Nil else acc :: Nil   //forgetting acc here was -3
      case Some(c) => if (c.isDigit)  {
         extractInts1(str.tail, acc+c)
      } else if (acc == "") {
         extractInts1(str.tail, "")
      } else {
         acc :: extractInts1(str.tail, "")
      }
   }


   /** Optional variant using a List[Char] input and giving a List[Int] output
    */
   def extractInts2(cl: List[Char], acc: String = ""): List[Int] = cl match {
      case Nil => if (acc == "") Nil else (acc.toInt) :: Nil   
      case c::rest => if (c.isDigit)  {
         extractInts2(rest, acc+c)
      } else if (acc == "") {
         extractInts2(rest, "")
      } else {
         (acc.toInt) :: extractInts2(rest, "")
      }
   }

      
   /** Version with no recursion and no match-case---and acc could be axed, too.
       Most common bug with this style was forgetting "mutacc" (or whatever called)
       after the for-loop exits: if the last char was a digit, it got saved into the
       accumulator but you still need to output it.  A point here is that the for-loop
       doesn't have a natural end-stage the way Nil provides in a list recursion.
    */
   def extractInts3(str: String, acc: String = ""): List[String] = {
      var ret: List[String] = Nil              //type qualifier is needed here
      var mutacc = acc                         //(no points deducted for that)
      for (c <- str) {
         if (c.isDigit) {
            mutacc += c
         } else if (mutacc != "") {
            ret ::= mutacc   //note: next element goes in front, so we have to reverse
            mutacc = ""
         }
      }
      if (mutacc != "") { ret ::= mutacc }     //forgetting this was -3
      return ret.reverse                       //forgetting this was -1 just
   }
 
   println("On input \"" + str + "\":")
   println("Answer 1 gives " + extractInts1(str))
   println("Answer 2 gives " + extractInts2(explode(str)))
   println("Answer 3 gives " + extractInts3(str))

   //The most common large error was listing each digit singly, so you would get
   //5 :: 0 :: 0 :: 7 :: 2 :: 2 :: 7 :: Nil
   //This was graded as -3 for a mistake of (not) accumulating and -4 for an issue
   //of control of execution in either a loop or in recursion, -7 in all.
   //

}
