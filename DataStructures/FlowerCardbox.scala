/** File "FlowerCardbox.scala", by KWR for CSE250, Spring 2022.
    Example test driver for all implementations of the ISR.scala API.
    Requires ISR.scala, Cardbox.scala, and the Cardbox implementation compiled in the same folder.
    Besides implementing ISR, the implemented class also needs a "diagnosticString" method in
    order to run the current debug-print code (as of Mar. 27, 2022 release),

    SPECIAL SCALA ELEMENTS:
    (1) Although "keyComp" is defined as a method, it is passed in using lambda ("rocket") syntax
        as a "flat function"---this is a neat savings over turgid C++ pointer-to-method syntax.
    (2) Note as usual that box1 and box2 being "val" does not prevent their being changed...
 */

class Flower(val name: String, val perennial: Boolean,
             val peakMonth: String, var colors: List[String]) {
   assert(!colors.isEmpty, "Attempt to create colorless flower")

   def keyMatch(other: Flower): Boolean = { name == other.name }
   def keyComp(other: Flower): Int = { name.compareTo(other.name) }

   override def toString: String = {                  //uses List iterator
      val p = if (perennial) "P" else "A"
      var itr = colors.iterator
      var c = itr.next()
      while (itr.hasNext) { c += "," + itr.next() }
      return s"$name ($p) $peakMonth; $c"
   }
}



//class Flowerbox extends Cardbox[Flower]((f1,f2) => f1.keyMatch(f2)) {
class Flowerbox extends Cardbox[Flower]((f1,f2) => f1.keyComp(f2)) {
   override def toString: String = {         //uses implementation's own Iter
      if (isEmpty) { return "Empty flowerbox" }
      val itr = begin
      var ret = "[" + itr.next().name
      while (itr.hasNext) { ret += "," + itr.next().name }
      return ret + "]"
   }
}

object FlowerCardbox extends App {
   val rose = new Flower("Rose", true, "June", List("red","white","pink","yellow"))
   val sunflower = new Flower("Sunflower", false, "August", List("yellow"))
   val tulip = new Flower("Tulip", true, "May", List("all colors"))
   val crocus = new Flower("Crocus", true, "March", List("blue", "yellow", "white"))
   val tigerlily = new Flower("Tiger Lily", true, "July", List("orange"))

   val box1 = new Flowerbox

   //box1.insert(sunflower, box.begin)
   //box1.insert(rose, box.end)
   box1.insert(sunflower)
println("Box1 now has " + box1)
println(box1.diagnosticString)
   box1.insert(rose)
   //box1.insert(sunflower)
   println("Box1 now has " + box1)
println(box1.diagnosticString)

   println("\nDoing Box 2 from scratch now:\n")
   val box2 = new Flowerbox
   //val itr2 = box2.insert(crocus, box2.begin)
   //val itr3 = box2.insert(tigerlily, itr2)
   //box2.insert(tulip, itr3)
   box2.insert(crocus)
println(box2.diagnosticString)
   box2.insert(tigerlily)
println(box2.diagnosticString)
   box2.insert(tulip)
   println("Box2 now has " + box2 + "\n" + box2.diagnosticString)
   println("")


   println("Putting box2 into box1, which is:\n")
println(box1.diagnosticString)
   //box1 ++= box2
   var itr = box2.begin
   while (itr.hasNext) { box1.insert(itr.next()); println(box1.diagnosticString) }
   println("The first box now has " + box1)
println(box1.diagnosticString)
   println("And box2 has " + box2)

   var itrf = box2.find(crocus)
println("On finding crocus, box2 has\n" + box2.diagnosticString)
   box2.remove(itrf)
println("Removed crocus to get\n" + box2.diagnosticString)
   itrf = box2.find(tigerlily)
println("Found tigerlily in\n" + box2.diagnosticString)
   box2.remove(itrf)
println("Removed tigerlily")
   itrf = box2.find(rose)
println("Found the name of the rose in " + box2.diagnosticString)
   try {
      box2.remove(itrf)
   } catch {
      case ex: java.lang.AssertionError => println("Hopefully no-op is OK.")
   }
   itrf = box2.find(tulip)
	   box2.remove(itrf)
   println("Box2 now: " + box2)

   //box2.insert(rose, box2.begin)
   //box2.insert(sunflower, box2.begin)
   box2.insert(rose)
   box2.insert(sunflower)
   println("Box2 after inserting rose and sunflower: " + box2)
   
}
