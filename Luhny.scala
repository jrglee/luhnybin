object Luhny {
  private val valueMap = Map(0 -> 0, 1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8, 5 -> 1, 6 -> 3, 7 -> 5, 8 -> 7, 9 -> 9)

  def mask(cc:String):String = {

    def maskImpl(cc:String):String = {
      
      def luhn(start:Int, end:Int, sum:Int, cc:List[Boolean], values:List[Int]) : List[Boolean] = {
        if (sum % 10 == 0) {
          val masked = cc.zipWithIndex map{case (e, i) => if (start <= i && i <= end) true else e} toList;
          if (end <= cc.size - 3) {
            var adjustedSum = sum + values(end + 1) + values(end + 2);
            for (i <- start until (end - 11))
              adjustedSum -= values(i)
            luhn(end - 11, end + 2, adjustedSum, masked, values) // advance to next 16
          } else
            masked
        } else if (end - start >= 14) {
          luhn(start + 1, end, sum - values(start), cc, values)
        } else if (end <= cc.size - 3) {
          luhn(start, end + 2, sum + values(end + 1) + values(end + 2), cc, values)
        } else {
          cc
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
  
    // determine index of valid numbers
    val digits = 
      for {
        i <- 0 to cc.size - 1
        c = cc.charAt(i)
        if ('0' <= c && c <= '9')
      } yield i
    
    if (digits.size >= 14) {
      val masked = new StringBuilder(cc)
      // call mask function and use them to replace the characters in the original string
      maskImpl(digits map {cc.charAt(_)} mkString).zipWithIndex map {case (c, i) => masked.setCharAt(digits(i), c)}
      // return value from string builder
      masked toString
    } else 
      cc
  }

  def main(args:Array[String]) {
    // windows keep appending \r if using println, this should be good enough to run everywhere
    Iterator.continually(Console.readLine).takeWhile(_ != null).foreach(ln => print(mask(ln) + "\n"))
  }
}
