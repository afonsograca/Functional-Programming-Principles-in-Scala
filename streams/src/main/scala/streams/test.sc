package streams

import streams._

object test {



	val s = Stream((1,2),(2,3),(2,1))         //> s  : scala.collection.immutable.Stream[(Int, Int)] = Stream((1,2), ?)
	
	s.head._2                                 //> res0: Int = 2

}