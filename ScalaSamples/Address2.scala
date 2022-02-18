/** File "Address2.scala" by KWR for CSE250, Spring 2022
    Shows case classes versus objects and standard classes.
 */
case class Address (           //no braces! Defines constructor at same time
    street1: String,           //mandatory address line
    street2: Option[String],   //since second street-address line often absent
    city: String, 
    state: String, 
    zip: String
) {
    override val toString = s"$street1, $street2, $city $state $zip" 
}

//class Test extends App    //compiler compains about lack of "static"
object Test2 extends App {
    var a = Address("10 Downing Street", None, "Westminster", "ENG", "SW1")
    //a.city = "London"
    println(a.toString)
    a = a.copy(city = "London")
    println(a.toString)
}
