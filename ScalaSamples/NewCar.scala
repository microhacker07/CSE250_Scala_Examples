/**File NewCar.scala.  Edited by KWR from Geeks For Geeks example at
   https://www.geeksforgeeks.org/trait-linearization-in-scala
   (Edits use camelCase and change "Paint" to "Finish" to be less like "Part")
   Shows multiple Scala traits with linear lookup order.
 */
class OldCar {
    def kind: String = "old car "
}
  
trait NewCarDesigns extends OldCar {  //legal since OldCar is treatable as trait
    override def kind: String = "Designing -> " + super.kind
}
  
trait NewCarPart extends OldCar
{
    override def kind: String = "Add new part -> " + super.kind
}
  
trait NewCarFinish extends OldCar
{
    override def kind: String = "Finishing -> " + super.kind
}
  
class NewCar extends NewCarFinish with NewCarPart with NewCarDesigns
{
    override def kind: String = "new car -> " + super.kind
}
  
object NewCar extends App {
    val car1 = new NewCar
    println(car1.kind)
}

/* Which "kind" method is looked up first?  If the "extend"ed trait had precedence,
it would be NewCarFinish.  But instead it is the last trait listed, so the message
about designing comes first.  The weirder part (IMPHO) is that super.kind in
NewCarDesigns is not bound to OldCar.  Instead, it refers to the superclass in the
linearization order created for the class NewCar.
*/
