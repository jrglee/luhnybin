import scala.collection.mutable.MutableList

object Luhny {
	private val valueMap = Map(0 -> 0, 1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8, 5 -> 1, 6 -> 3, 7 -> 5, 8 -> 7, 9 -> 9)

  def mask(cc:String):String = {

		def maskImpl(cc:String):String = {
			
			def luhn(start:Int, end:Int, sum:Int, cc:List[Boolean], values:List[Int]) : List[Boolean] = {
				val newRanges = {
					if (sum % 10 == 0)
						cc.zipWithIndex map{case (e, i) => if (start <= i && i <= end) true else e} toList
					else
						cc
				}
				
				if (end - start >= 14) {
					luhn(start + 1, end, sum - values(start), newRanges, values)
				} else if (end <= cc.size - 3) {
					luhn(start, end + 2, sum + values(end + 1) + values(end + 2), newRanges, values)
				} else {
					newRanges
				}
			}
			
			def emptyList(size:Int) :List[Boolean] = 0 until size map {e => false} toList

			// convert string to list of ints
			val baseValueList = cc map {c => (c - '0').toInt} toList;
			
			// list with even indexes doubled
			val evens = baseValueList.zipWithIndex map {case (e,i) => if (i % 2 == 0) valueMap(e) else e} toList;
			
			// list with odd indexes doubled
			val odds = baseValueList.zipWithIndex map {case (e,i) => if (i % 2 == 0) e else valueMap(e)} toList;
			
			// get all possible ccs with list of evens
			val charMap1 = luhn(0, 13, evens.take(14).sum, emptyList(cc.size), evens)
			
			// same with list of odds
			val charMap2 = {
				if (cc.size >= 15)
					luhn(0, 14, odds.take(15).sum, emptyList(cc.size), odds) 
				else
					emptyList(cc.size)
			}
			
			cc.zipWithIndex map{case (c, i) => if (charMap1(i) || charMap2(i)) 'X' else c} mkString;
		}
	
		if (cc.size < 14)
			return cc
	
		val cleanStr = new StringBuilder()
		val digits = new MutableList[Int]()
		
		for (i <- 0 until cc.size) {
			if ('0' <= cc.charAt(i) && cc.charAt(i) <= '9'){
				cleanStr.append(cc(i))
				digits += i
			}
		}
		
		if (cleanStr.size >= 14) {
			val masked = new StringBuilder(cc)
			maskImpl(cleanStr.toString).zipWithIndex map {case (c, i) => masked.setCharAt(digits(i), c)}
			masked toString
		} else cc
  }

  def main(args:Array[String]) {
    for (ln <- io.Source.stdin.getLines)
      print(mask(ln) + "\n")
  }
}

