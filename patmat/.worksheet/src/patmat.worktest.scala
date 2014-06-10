package patmat

object worktest {
  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char,val weight: Int) extends CodeTree;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(828); 



  // Part 1: Basics

  def weight( tree: CodeTree): Int = tree match {
    case s: Leaf => s.weight
    case s: Fork => s.weight
  };System.out.println("""weight: (tree: patmat.worktest.CodeTree)Int""");$skip(121); 

  def chars(tree: CodeTree): List[Char] = tree match {
    case s: Leaf => List(s.char)
    case s: Fork => s.chars
  };System.out.println("""chars: (tree: patmat.worktest.CodeTree)List[Char]""");$skip(137); 

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right));System.out.println("""makeCodeTree: (left: patmat.worktest.CodeTree, right: patmat.worktest.CodeTree)patmat.worktest.Fork""");$skip(262); 



  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList;System.out.println("""string2Chars: (str: String)List[Char]""");$skip(1225); 

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    def pack(xs: List[Char]): List[List[Char]] = xs match{
      case Nil => Nil
      case x::xs1 =>
        val(first, rest) = xs span (y => y == x)
        first::pack(rest)
    }
    pack(chars.sorted) map (ys =>(ys.head,ys.length))
  };System.out.println("""times: (chars: List[Char])List[(Char, Int)]""");$skip(418); 

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs.sortBy(pair => pair._2).map(pair => Leaf(pair._1, pair._2));System.out.println("""makeOrderedLeafList: (freqs: List[(Char, Int)])List[patmat.worktest.Leaf]""");$skip(224); 

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case x::Nil => true
    case Nil => false
    case x::xs => false
  };System.out.println("""singleton: (trees: List[patmat.worktest.CodeTree])Boolean""");$skip(776); 

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case x::xs => {
      val newTree = makeCodeTree(x, xs.head)
      newTree :: xs.tail.filter(tree => weight(tree) >= weight(newTree)):::xs.tail.filter(tree => weight(tree) < weight(newTree))
    }
    case _ => trees
  };System.out.println("""combine: (trees: List[patmat.worktest.CodeTree])List[patmat.worktest.CodeTree]""");$skip(1033); 

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  def until(breaker: List[CodeTree] => Boolean, iterator: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = breaker(trees) match {
    case true => trees
    case false => until(breaker,iterator)(iterator(trees))
  };System.out.println("""until: (breaker: List[patmat.worktest.CodeTree] => Boolean, iterator: List[patmat.worktest.CodeTree] => List[patmat.worktest.CodeTree])(trees: List[patmat.worktest.CodeTree])List[patmat.worktest.CodeTree]""");$skip(374); 

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = until(singleton,combine)(makeOrderedLeafList(times(chars)))(0);System.out.println("""createCodeTree: (chars: List[Char])patmat.worktest.CodeTree""");$skip(58); val res$0 = 

	(createCodeTree(List('s','o','m','e','T','e','x','t')));System.out.println("""res0: patmat.worktest.CodeTree = """ + $show(res$0))}

}
