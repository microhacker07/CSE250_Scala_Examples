/** File "AddressTest.scala" by KWR for CSE250, Spring 2022
    
 */
class Address (                    //no braces! Defines constructor at same time
    var street1: String,           //mandatory
    var street2: Option[String],   //since second street-address line often absent
    var city: String, 
    var state: String, 
    var zip: String
)

object AddressTest extends App {
    val houseadr = new Address("10 Downing Street", None, "Amherst", "NY", "14226")
    val deptadr = new Address("338 Davis Hall", Some("UB North Campus"), "Amherst", "NY", "14260")
    println(s"Home address second line is ${houseadr.street2}")
    println(s"Work address second line is ${deptadr.street2}")
    var s1 = houseadr.street1        //makes Scala infer s1: String, so that
    s1 = houseadr.street2          //is streng verboten.  Whereas, a null reference
                                     //in Java would silently propagate.
    //s1 = houseadr.street2.toString   //no () needed
    println(s"toString of Option[String] prints None case as $s1")
    s1 = s"toString of Option[String] prints None case as $s1"
    s1 = deptadr.street2 match {
        case None => ""  
        case Some(s) => s
    }
    println(s"Work address second line cleaned up is $s1")
}
