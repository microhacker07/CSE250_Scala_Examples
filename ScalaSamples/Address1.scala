/** File "Address1.scala" by KWR for CSE250, Spring 2022.  
    Adds non-override method "address" to illustrate functional programming in Scala.
 */
class Address1 (                   //no braces! Defines constructor at same time
    var street1: String,           //mandatory address line
    var street2: Option[String],   //since second street-address line often absent
    var city: String, 
    var state: String, 
    var zip: String
) {
    override def toString = s"$street1, $street2, $city $state $zip" 

    def address(give2nd: Boolean) = {
        if (give2nd) {
            street2 match {
                case None => s"$street1, $city $state $zip"
                case Some(s2) => s"$street1, $s2, $city $state $zip"
            }
        } else {
            s"$street1, $city $state $zip"
        }
    }
}


object Address1 extends App {

    /** A generic higher-order function
     */
    def dynaprint[T](obj:T, fun:T => String) = {  
       println(fun(obj))
    }

    var pm = new Address1("10 Downing Street", Some("Party!!!"), "Westminster", "ENG", "SW1")

    dynaprint(pm, (a:Address1) => a.address(false))

    pm.city = "London"

    dynaprint(pm, (a:Address1) => a.address(true))
}
