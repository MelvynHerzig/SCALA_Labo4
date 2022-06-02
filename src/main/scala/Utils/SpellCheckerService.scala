package Utils

import scala.annotation.tailrec

trait SpellCheckerService:
  /**
    * This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
    * we want to normalize the words "veux" and "aimerais" in one unique term: "vouloir").
    */
  val dictionary: Map[String, String]

  /**
    * Function that checks if the given string is a number not necessarily integer nor positive.
    *
    * @param s String to check if it is a number
    * @return True if the string is a number else False.
    */
  def isNumber(s: String): Boolean

  /**
    * Function that checks if the given string is a pseudonym (i.e: it starts with an _ )
    *
    * @param s
    * @return
    */
  def isPseudonym(s: String): Boolean

  /**
    * Calculate the Levenstein distance between two words.
    *
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenstein distance between "s1" and "s2"
    */
  def stringDistance(s1: String, s2: String): Int

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number or a pseudonym, this function just returns it.
    *
    * @param misspelledWord the mispelled word to correct
    * @return the closest normalized word from "mispelledWord"
    */
  def getClosestWordInDictionary(misspelledWord: String): String
end SpellCheckerService

class SpellCheckerImpl(val dictionary: Map[String, String]) extends SpellCheckerService :

  def isNumber(s: String): Boolean =
    s.matches("[+-]?\\d?\\d+")
  end isNumber

  def isPseudonym(s: String): Boolean =
    s != "" && s.charAt(0) == '_'
  end isPseudonym

  def stringDistance(s1: String, s2: String): Int =

    // This algorithm is implemented by translating the iterative version
    // From this video (https://www.youtube.com/watch?v=We3YDTzNXEk&ab_channel=TusharRoy-CodingMadeSimple)
    // into a tail recursive version.

    // The word s1 will form the rows of the matrix and the word s2 the columns
    // Observation:
    //    - The first row always goes from 0 to s2.length
    //    - The first column always goes from 0 to s1.length

    // Behavior:
    //    - On each iteration of the tail recursion, the value to compute is 1 + min(left cell, top-left cell, top cell)
    //    - The final distance = the value of the last computed cell.

    val upperCache = 0.to(s2.length).toArray // Correspond to the first row [0][0 : S2.length]
    // Slowly populated to represent next rows.

    var leftCache = 1 // Correspond to the left cell [1][0] of the first cell to compute
    // Change at each iteration after being saved on the upper cache for the
    // next row.

    var currentValue = -1 // The value of the cell we are currently computing.

    /**
      * recursive function to compute Levenshtein's distance matrix
      *
      * @param s1 Word whose letters represent rows.
      * @param s2 Word whose letters represent columns.
      * @param l1 from 0 to l1, part of word s1 that we comparing with 0 to l2 from word s2
      * @param l2 from 0 to l2, part of word s2 that we comparing with 0 to l1 from word s1
      * @return The Levenshtein's distance (bottom right cell of matrix)
      */
    @tailrec
    def sd(s1: String, s2: String, l1: Int, l2: Int): Int =

      // When we work on the first letter of s2, the left cell value is always l1 + 1.
      if l2 == 0 then
        leftCache = l1 + 1

      // -------- Computing current cell ------------

      // If the s1 char at l1 equals s2 char at l2, the value of the current cell is always equals to the cell on the
      // top left from the current cell we are working on.
      //
      // Otherwise, the current cell value equals 1 + min(left cell, top-left cell, top cell)
      if s1.charAt(l1) == s2.charAt(l2) then
        currentValue = upperCache(l2)
      else
        currentValue = leftCache.min(upperCache(l2).min(upperCache(l2 + 1))) + 1

      // Saving left cell into top row cache in prevision of handling the next row.
      upperCache(l2) = leftCache
      leftCache = currentValue

      if l2 < s2.length - 1 then // If last column is not reached, go to the next column.
        sd(s1, s2, l1, l2 + 1)
      else if l1 < s1.length - 1 then // If last row not reach, go to next row from column 0.
        upperCache(l2 + 1) = currentValue
        sd(s1, s2, l1 + 1, 0)
      else // The matrix is full, we return the final value.
        currentValue
    end sd

    // Filtering with match because sd doesn't handle "" string.
    (s1, s2) match
      case ("", "") => 0
      case ("", s2) => s2.length
      case (s1, "") => s1.length

      // Start filling matrix from cell [1][1] (since we already know first row and first column)
      case (s1, s2) => sd(s1, s2, 0, 0)

  end stringDistance

  def getClosestWordInDictionary(misspelledWord: String): String =

  // If the word is a number or pseudo return as it is.
    if isNumber(misspelledWord) || isPseudonym(misspelledWord) then misspelledWord
    else {

      dictionary(dictionary.foldLeft(("", Int.MaxValue))((best, current) => {
        val distance = stringDistance(current._1, misspelledWord) // Levenshtein's distance
        if distance < best._2 then // If the distance is better than saved one
          (current._1, distance) // Save the current key
        else if distance == best._2 && current._1 < best._1 then // If they are equal but the key is smaller than the saved one.
          (current._1, distance) // Save the current key
        else
          (best._1, best._2)
      })._1)

    }

end SpellCheckerImpl
