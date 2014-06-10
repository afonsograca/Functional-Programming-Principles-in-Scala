package streams

import streams._

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(86); 



	val s = Stream((1,2),(2,3),(2,1));System.out.println("""s  : scala.collection.immutable.Stream[(Int, Int)] = """ + $show(s ));$skip(13); val res$0 = 
	
	s.head._2;System.out.println("""res0: Int = """ + $show(res$0))}

}
