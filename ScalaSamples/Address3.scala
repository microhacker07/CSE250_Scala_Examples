/** File "Address3.scala" by KWR for CSE250, Spring 2022
    ??
 */
class Address3 (                   //no braces! Defines constructor at same time
    var street1: String,           //mandatory address line
    var street2: Option[String],   //since second street-address line often absent
    var city: String, 
    var state: String, 
    var zip: String
) {
    override def toString = s"$street1, $street2, $city $state $zip" 
}


object Address3 extends App {

    def dynatest[T](obj:T, fun:T => String) = {
       println(fun(obj))
    }

    def aString(addr:Address3) = addr.toString

    var a = new Address3("10 Downing Street", None, "Westminster", "ENG", "SW1")

    dynatest(a,aString)

    a.city = "London"

    dynatest(a,aString)
}
