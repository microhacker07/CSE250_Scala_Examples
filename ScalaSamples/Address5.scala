class Address5 (                   //no braces! Defines constructor at same time
    var street1: String,           //mandatory address line
    var street2: Option[String],   //since second street-address line often absent
    var city: String, 
    var state: String, 
    var zip: String
) {
    override def toString = s"$street1, $street2, $city $state $zip" 

    def address(give2nd: Boolean) = {
    //val address = (give2nd: Boolean) => {
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


object Address5 extends App {

    def simpleAddress(a5:Address5) = a5.address(false)
    //val simpleAddress = (a5:Address5) => a5.address(false)
    //val simpleAddress = a5 => a5.address(false)
    //val simpleAddress: Address5 => String = a5 => a5.address(false)

    def dynaprint[T]

    val a = new Address5("10 Downing Street", None, "Westminster", "ENG", "SW1")

    println(simpleAddress(a))

    a.city = "London"

    println(simpleAddress(a))
}
