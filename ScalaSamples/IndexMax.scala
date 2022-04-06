object IndexMax extends App {
   def indexMax(arr: Array[String]):Int = {
      var (max, indexmax) = ("", -1)
      for (i <- 0 until arr.length) { //until is exclusive
         if (arr(i) > max) {
            max = arr(i);
            indexmax = i 
         }
      } 
      indexmax
   }
   //val arr = Array(3,24,5,17,8)
   val arr = "The quick brown fox jumped over    that	lazy dog's back".split("\\s+")
   val i = indexMax(arr)
   println(s"The index of the alpha max word is $i in the list ${arr.toList}")
}
