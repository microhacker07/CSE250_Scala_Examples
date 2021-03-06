/** File "AHquiz.scala" by KWR for CSE250, Spring 2022. Source: O. Kennedy's "AI Quiz"
    Paraphrase of questions on the quiz, plus some others.
    Shows basic screen input and file output, as well as implementing the quiz.
    For Week 2 recitations, to be submitted as test-run to CSE Autograder
 */
import io.StdIn._
import java.io.File
import java.io.FileWriter   //makes it easy to append
import java.io.PrintWriter  //makes "print" and "println" available

object AHquiz extends App {
   var answers: List[String] = Nil    //note: the "var" allows appending even though List is immutable

   println("\nPlease answer true/false questions with \"True\" or \"False\" and multi-choice with capital letter(s).")
   println("Some parts of the multi-choice questions have shades of gray; answer them as best you can.")
   println("[\"Multi-choice\" means more than one of the letter items can be correct, or none.]")

   println("\n(1) True/False? If you use a paragraph from a source and cite it correctly,")
   print("    then you cannot be guilty of plagiarism.  ")
   var answer = readLine()
   answers ::= answer


   println("\n(2) True/False? If you are accused of an academic integrity violation,")
   print("    then you may opt to immediately drop the course.  ")
   answer = readLine()
   answers ::= answer


   println("\n(3) True/False? A second violation of academic integrity shall bring an F in the course,")
   print("    even if the previous violation was in another course.  ")
   answer = readLine()
   answers ::= answer


   println("\n(4) True/False? University policy specifies that all academic integrity matters in a course remain")
   print("    within that course, and the instructor's judgment of them is final.  ")
   answer = readLine()
   answers ::= answer


   println("\n(5) True/False? If I think I may have been party to an academic integrity violation,")
   print("    voluntarily bringing it to the attention of instructors cannot help my situation.  ")
   answer = readLine()
   answers ::= answer

   println("\n(6) True/False? An instructor making an academic dishonesty accusation must have a")
   print("    University official present at the first meeting with the accused student(s).  ")
   answer = readLine()
   answers ::= answer

   println("\n(7) True/False? If an assignment problem in CSE250 was an extra-credit option in CSE116 or")
   print("    similar course, then University rules allow me to submit my earlier answer for credit.  ")
   answer = readLine()
   answers ::= answer

   println("\n(8) Which of the following resources are in-bounds for use on an assignment, with attribution?")
   println("    (A) Code or links shown in lecture to support the assignment")
   println("    (B) Code or links posted by a UTA on Piazza, whether or not the instructor has clicked 'good answer' on it.")
   println("    (C) Code or links posted by a student on Piazza, whether or not the instructor has clicked 'good answer' on it.")
   println("    (D) CourseHero or Chegg or similar site, if you have paid for it.")
   println("    (E) The official online Scala language and API documentation, including the Scala 3 Book.")
   println("    (F) StackOverflow or Reddit")
   println("    (G) GitHub Copilot")
   println("    (H) Wikipedia")
   print("    Write the letters of all that apply as one string: ")
   answer = readLine()
   answers ::= answer

   println("\n(9) Which of the following are legitimate discussion topics in a study group?")
   println("    (A) How to get syntax for needed Java utilities, like writing files in this code.")
   println("    (B) Examples in the textbook that could be relevant but the instructor didn't cover them in class.")
   println("    (C) Which of several answers on StackOverflow is most appropriate to this assignment.")
   println("    (D) An exact sequence of steps to complete an assignment.")
   println("    (E) How to deal with unexpected compiler errors.")
   print("    Write the letters of all that apply as one string: ")
   answer = readLine()
   answers ::= answer


   println("\n(10) If a friend needs extra help in the course, then I should:")
   println("    (A) Arrange special tutoring for the friend, keeping it secret for privacy.")
   println("    (B) Encourage the friend to talk to one of the instructional staff about the issues.")
   println("    (C) Tell the instructor or others in the instructional staff about the issues.")
   println("    (D) Pay for the friend's subscription to Chegg.com.")
   println("    (E) Suggest sketching the code in Python or Java or Javascript-like syntax, since Scala is so complicated.")
   print("    Write the letters of all that apply as one string: ")
   answer = readLine()
   answers ::= answer

   answers = answers.reverse   //!! Because Scala ::= prepends.  See text p36 for similar need.
   val indexedAnswers = List.tabulate(10)(_+1).zip(answers)

   val filep = new PrintWriter(new FileWriter("AHquiz.scala",true));  //appends
   filep.println("\n//My answers: " + indexedAnswers.mkString(","))
   filep.close()
}

