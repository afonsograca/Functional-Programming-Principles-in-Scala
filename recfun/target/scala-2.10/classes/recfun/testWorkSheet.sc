package recfun

object testWorkSheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  def balance(chars: List[Char]): Boolean = {
    
    var i = 0
    def balance_aux(chars: List[Char], sum: Int): Int = {
      var x = sum
      if(chars.isEmpty)
        return sum
      else{
        if( chars.head == '(')
         x = x + 1
        else if (chars.head == ')')
          x = x -1
     
        return balance_aux(chars.tail, x)
      }
    }
    i = balance_aux(chars,i)
    
    if(i == 0)
      return true
    else
      return false
  }                                               //> balance: (chars: List[Char])Boolean
  balance(":-)".toList)                           //> res0: Boolean = false
}