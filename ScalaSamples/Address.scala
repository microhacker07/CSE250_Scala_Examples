/** File "Address.scala" by KWR for CSE250, Spring 2022.
    Shows effect of modifying a mutable field
 */
class Address (                    //no braces! Defines constructor at same time
    var street1: String,           //mandatory address line
    var street2: Option[String],   //since second street-address line often absent
    var city: String, 
    var state: String, 
    var zip: String
) {
    override def toString = { s"$street1, $street2, $city $state $zip" }
}

object Address extends App {
    val a = new Address("10 Downing Street", None, "Westminster", "ENG", "SW1")
    a.city = "London"
    println(a.toString)
}
