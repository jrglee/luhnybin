import scala.collection.mutable.MutableList

object Luhny {
  private val valueMap = Map(0 -> 0, 1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8, 5 -> 1, 6 -> 3, 7 -> 5, 8 -> 7, 9 -> 9)

  def mask(cc:String):String = {

    def maskImpl(cc:String):String = {
      
      def luhn(start:Int, end:Int, sum:Int, cc:List[Boolean], values:List[Int]) : List[Boolean] = {
        val masked : List[Boolean] = {
          if (sum % 10 == 0)
            cc.zipWithIndex map{case (e, i) => if (start <= i && i <= end) true else e} toList
          else
            cc
        }
        
        if (end - start >= 14) {
          luhn(start + 1, end, sum - values(start), masked, values)
        } else if (end <= cc.size - 3) {
          luhn(start, end + 2, sum + values(end + 1) + values(end + 2), masked, values)
        } else {
          masked
        }
      }

      // convert string to list of ints
      val baseValueList = cc map {c => (c - '0').toInt} toList;
      
      // list with even indexes doubled
      val evens = baseValueList.zipWithIndex map {case (e,i) => if (i % 2 == 0) valueMap(e) else e} toList;
      
      // get all possible ccs with list of evens
      val partialMask = luhn(0, 13, evens.take(14).sum, List.fill[Boolean](cc.size)(false), evens)
      
      // same with list of odds
      val mask = {
        if (cc.size >= 15) {
          // list with odd indexes doubled
          val odds = baseValueList.zipWithIndex map {case (e,i) => if (i % 2 == 0) e else valueMap(e)} toList;
          luhn(0, 14, odds.take(15).sum, partialMask, odds) 
        } else
          partialMask
      }
      
      // replace numbers by Xs using boolean maps as references
      cc.zipWithIndex map{case (c, i) => if (mask(i)) 'X' else c} mkString;
    }
  
    // return if string is not long enough to do anything
    if (cc.size < 14)
      return cc
  
    // isolate numbers only
    val cleanStr = new StringBuilder

    // stores the index of numeric chars
    val digits = new MutableList[Int]
    
    // TODO - rewrite in functional style
    for (i <- 0 until cc.size) {
      if ('0'.to('9').contains(cc.charAt(i))){
        cleanStr.append(cc(i))
        digits += i
      }
    }
    
    if (cleanStr.size >= 14) {
      // TODO rewrite in functional style
      val masked = new StringBuilder(cc)
      maskImpl(cleanStr.toString).zipWithIndex map {case (c, i) => masked.setCharAt(digits(i), c)}
      masked toString
    } else 
      cc
  }

  def main(args:Array[String]) {
    for (ln <- io.Source.stdin.getLines)
      // windows keep appending \r if using println, this should be good enough to run everywhere
      print(mask(ln) + "\n")
  }
}

