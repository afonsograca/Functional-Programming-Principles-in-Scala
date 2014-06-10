package forcomp

object worksheet {
  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)];import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(757); 
  val sentence = List("Linux", "rulez");System.out.println("""sentence  : List[String] = """ + $show(sentence ));$skip(198); 

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary;System.out.println("""dictionary  : List[forcomp.worksheet.Word] = """ + $show(dictionary ));$skip(370); 

  /** Converts the word into its character occurence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences =
    (w.toLowerCase() groupBy(x => x) mapValues (y => y.length) toList) sorted;System.out.println("""wordOccurrences: (w: forcomp.worksheet.Word)forcomp.worksheet.Occurrences""");$skip(156); 

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences(s mkString(""));System.out.println("""sentenceOccurrences: (s: forcomp.worksheet.Sentence)forcomp.worksheet.Occurrences""");$skip(32); val res$0 = 

	sentenceOccurrences(sentence);System.out.println("""res0: forcomp.worksheet.Occurrences = """ + $show(res$0));$skip(779); 
  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    ((dictionary map (x => (wordOccurrences(x), x))).groupBy(x => x._1)) mapValues ( y => y map(_._2));System.out.println("""dictionaryByOccurrences: => Map[forcomp.worksheet.Occurrences,List[forcomp.worksheet.Word]]""");$skip(166); 

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)).getOrElse(List());System.out.println("""wordAnagrams: (word: forcomp.worksheet.Word)List[forcomp.worksheet.Word]""");$skip(1036); 

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] =
    List() :: (for {
      (xs, ys) <- occurrences
      i       <- 1 to ys
      j       <- combinations(occurrences.filter(pair => pair._1 > xs))
    } yield List((xs, i)) ++ j);System.out.println("""combinations: (occurrences: forcomp.worksheet.Occurrences)List[forcomp.worksheet.Occurrences]""");$skip(615); 
    
  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = (for {
    (xi,xj) <- x
    if xj > (y.toMap.get(xi)).getOrElse(0)
  } yield (xi,xj-(y.toMap.get(xi)).getOrElse(0))) sorted;System.out.println("""subtract: (x: forcomp.worksheet.Occurrences, y: forcomp.worksheet.Occurrences)forcomp.worksheet.Occurrences""");$skip(2231); 

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def anagrams(occurrences: Occurrences): List[Sentence] = {
      occurrences match{
      case List() => List(Nil)
      case _ => (for{
        xs <- combinations(occurrences)
        if(dictionaryByOccurrences.contains(xs))
        ys <- dictionaryByOccurrences.get(xs).get
        zs <- anagrams(subtract(occurrences, xs))
      	} yield List(ys) ++ zs)
      }
    }
    anagrams(sentenceOccurrences(sentence))
  };System.out.println("""sentenceAnagrams: (sentence: forcomp.worksheet.Sentence)List[forcomp.worksheet.Sentence]""");$skip(50); val res$1 = 

  
  combinations(sentenceOccurrences(sentence));System.out.println("""res1: List[forcomp.worksheet.Occurrences] = """ + $show(res$1));$skip(40); 
  
  val sentence1 = List("Yes", "man");System.out.println("""sentence1  : List[String] = """ + $show(sentence1 ));$skip(29); val res$2 = 
  sentenceAnagrams(sentence);System.out.println("""res2: List[forcomp.worksheet.Sentence] = """ + $show(res$2));$skip(70); 
     
  
  
  val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1));System.out.println("""lard  : List[(Char, Int)] = """ + $show(lard ));$skip(25); 
  val r = List(('r', 1));System.out.println("""r  : List[(Char, Int)] = """ + $show(r ));$skip(20); val res$3 = 
  subtract(lard, r);System.out.println("""res3: forcomp.worksheet.Occurrences = """ + $show(res$3))}
  
  
}
