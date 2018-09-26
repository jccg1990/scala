import forcomp.Anagrams.{Occurrences, Word, wordOccurrences}
import forcomp.loadDictionary

object playGround {
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy(w => wordOccurrences(w))
  /** The dictionary is simply a sequence of words.
    * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
    */
  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences = {
    w.toLowerCase.groupBy(x => x) map {
      case (y1, y2) => y1 -> y2.length
    }
  }.toList.sorted

}