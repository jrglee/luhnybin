object Luhny {
  def luhnCheck(cc:String):Boolean = {
    def asInt(c:Char):Int = c - '0'
    def luhnCheckImpl(sum:Int, cc:String, m:Int):Int = {
      if (cc == "")
        sum
      else 
        luhnCheckImpl(sum + (asInt(cc.head) * m).toString.foldLeft(0)((r,c) => r + asInt(c)), cc.tail, m % 2 + 1)
    }

    return luhnCheckImpl(0, cc.reverse.replaceAll("[\\s-]", ""), 1) % 10 == 0
  }


  def maskCc(cc:String):String = {
    if (14.to(16).contains(cc.size) && luhnCheck(cc)) 
      cc.map{s => 
        if (s.toString.matches("\\d"))
          'X' 
        else 
          s
      }
    else 
      cc
  }

  def main(args:Array[String]) {
    for (ln <- io.Source.stdin.getLines)
      print(maskCc(ln) + "\n")
  }
}

