package Chat

import Chat.Token.*
import Utils.SpellCheckerService

class TokenizerService(spellCheckerSvc: SpellCheckerService):
  /**
    * Function that return a token according to a word.
    *
    * @param word Word to translate into a token.
    * @return A value from Token enum.
    */
  def getCorrespondingToken(word: String): Token =
    word match
      case "bonjour" => Token.BONJOUR
      case "je" => Token.JE
      case "etre" => Token.ETRE
      case "vouloir" => Token.VOULOIR
      case "assoiffe" => Token.ASSOIFFE
      case "affame" => Token.AFFAME
      case "biere" => Token.PRODUCT
      case "croissant" => Token.PRODUCT
      case "et" => Token.ET
      case "ou" => Token.OU
      case "svp" => Token.SVP
      case "maison" => Token.MARQUE
      case "cailler" => Token.MARQUE
      case "farmer" => Token.MARQUE
      case "boxer" => Token.MARQUE
      case "wittekop" => Token.MARQUE
      case "punkipa" => Token.MARQUE
      case "jackhammer" => Token.MARQUE
      case "tenebreuse" => Token.MARQUE
      case "commander" => Token.COMMANDER
      case "connaitre" => Token.CONNAITRE
      case "solde" => Token.SOLDE
      case "mon" => Token.MON
      case "me" => Token.ME
      case "coûter" => Token.COUTER
      case "quel" => Token.QUEL
      case "le" => Token.LE
      case "prix" => Token.PRIX
      case "de" => Token.DE
      case "appeler" => Token.APPELER
      case "combien" => Token.COMBIEN
      case a if spellCheckerSvc.isPseudonym(a) => Token.PSEUDO
      case a if spellCheckerSvc.isNumber(a) => Token.NUM
      case _ => Token.UNKNOWN
  end getCorrespondingToken

  /**
    * Separate the user's input into tokens
    *
    * @param input The user's input
    * @return A Tokenizer which allows iteration over the tokens of the input
    */
  def tokenize(input: String): Tokenized =

    // Removing punctuation and trimming then split by space.
    val words = input.replaceAll("[.,!?*]", "")
      .replaceAll("[’']", " ")
      .trim.replaceAll(" +", " ")
      .toLowerCase()
      .split(" ")

    var tokens: Array[(String, Token)] = Array()

    // For each word, get the corresponding word from dictionary and add the pair into the tokens array.
    words.foreach(word => {
      val realWord: String = spellCheckerSvc.getClosestWordInDictionary(word)
      tokens = tokens :+ (realWord, getCorrespondingToken(realWord))
    })

    TokenizedImpl(tokens)
  end tokenize
end TokenizerService
