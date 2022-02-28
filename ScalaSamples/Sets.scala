/** File Sets.scala, by KWR for CSE250, Spring 2022
    An idea that is possible but not needed (nerdy overkill?) for Assignment 2
    Builds on https://stackoverflow.com/questions/7681183/how-can-i-define-a-custom-equality-operation-that-will-be-used-by-immutable-set
 */

//import scala.collection.mutable.Set
import scala.collection.immutable.Set

case class MyString(val name: String) {               //example of "composition not inheritance"

  override def equals(other: Any) = other match {     //KWR: match used as a type-test, ingenious.
    case that: MyString => that.name.equalsIgnoreCase(this.name)
    case _ => false
  }

  override def hashCode = name.toUpperCase.hashCode
}

object Sets extends App {
   val mySet = Set(MyString("conservative"), MyString("conversative"), MyString("Conservative"))
   println("Got " + mySet)
   //val mySet2 = Set[MyString]()
   //mySet2 += "xenon"
}

