/** File "Address2.scala" by KWR for CSE250, Spring 2022
    More-complicated version of Address1.scala
 */

class Address4 (                   //no braces! Defines constructor at same time
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


object Address4 extends App {

    /** A generic higher-order function
     */
    def dynaprint[T](obj:T, fun:T => String) = {  
       println(fun(obj))
    }

    /** A "closure" of the address method (not "curried")
     */
    def simpleAddress(a4:Address4) = a4.address(false) 

    var pmr = new Address4("10 Downing Street", None, "Westminster", "ENG", "SW1")

    dynaprint(pmr, simpleAddress)

    pmr.city = "London"

    dynaprint(pmr, simpleAddress)
}
