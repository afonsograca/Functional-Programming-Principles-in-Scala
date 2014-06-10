package recfun

object testWorkSheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(82); 
  println("Welcome to the Scala worksheet");$skip(462); 
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
  };System.out.println("""balance: (chars: List[Char])Boolean""");$skip(24); val res$0 = 
  balance(":-)".toList);System.out.println("""res0: Boolean = """ + $show(res$0))}
}
