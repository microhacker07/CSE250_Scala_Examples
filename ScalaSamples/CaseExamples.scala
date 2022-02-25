/** File CaseExamples.scala, by KWR for CSE250, Spring 2022
 */

object CaseExamples extends App {

   def isVowel(c: Char) = c match {
      case 'a' => true
      case 'e' => true
      case 'i' => true
      case 'o' => true
      case 'u' => true
      case _ => false
   }

   def isVowel2(c: Char): Boolean = c match {
      case 'a' | 'A' | 'e' | 'E' | 'i' | 'I' | 'o' | 'O' | 'u' | 'U' | 'y' | 'Y' => true
      case _ => false 
   }

   def vowelsIn(str: String): List[Boolean] = str.headOption match {
      case None => Nil
      case Some(c) => isVowel(c) :: vowelsIn(str.tail)
   }

   /** Make list of chars and whether they are vowels, as tuples (char, isVowel(char))
    */
   def vowelsIn2(str: String): List[(Char,Boolean)] = str.headOption match {
      case None => Nil
      case Some(c) => (c,isVowel2(c)) :: vowelsIn2(str.tail)
   }

   println("Which chars in \"AnyRef\" are vowels? " + vowelsIn2("AnyRef"))

   println("\n\n")

   /** Matching on tuples 
    */
   def numberType(tup: (Double,Double)): String = tup match {
      case (0.0, 0.0) => "the origin"
      case (_,   0.0) => "nonzero real number"
      case (0.0, _)   => "nonzero pure imaginary number"
      case (_, _)     => "mixed complex number"
   }
   
   val (t1, t2, t3, t4, t5) = ((0.0,0.0), (-1.0,0.0), (0.0,1.0), (2.0,2.0), (2.0,3.0))
   println("What kinds of complex numbers are these?")
   //println(t1 + ": " + numberType(t1))    //deprecation: implicit toString on LHS of +
   println("" + t1 + ": " + numberType(t1)) //useful trick: on RHS of "" + it is always fine
   println("" + t2 + ": " + numberType(t2))
   println("" + t3 + ": " + numberType(t3))
   println("" + t4 + ": " + numberType(t4))
   println("" + t5 + ": " + numberType(t5))
   
}


