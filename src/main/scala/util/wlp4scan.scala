package util

import definition.WLP4
import logic.DFA
import model.{Token, WLP4DFA}

object wlp4scan {
  /* An implementation of the Simplified Maximal Munch algorithm,
   * which will be presented in class around the time of Assignment 6.
   */
  def simplifiedMaximalMunch(dfa: DFA, input: List[Char]): Seq[Token] = {
    /* Munch the longest possible token and return it along with the remaining unconsumed input */
    def scanOne(input: List[Char], state: String = dfa.start, consumedInput: List[Char] = Nil): (List[Char], Token) =
      if (input.isEmpty || !dfa.transition.isDefinedAt((state,input.head)))
        if (dfa.accepting contains state) (input, Token(state, consumedInput.reverse.mkString))
        else sys.error(s"ERROR: simplified maximal munch failed on input: ${consumedInput.reverse ++ input}")
      else scanOne(input.tail, dfa.transition((state,input.head)), input.head :: consumedInput)

    /* Repeatedly call scanOne until the input is consumed or an error occurs */
    def scanAll(input: List[Char], accum: List[Token] = Nil): Seq[Token] = input match {
      case Nil => accum.reverse
      case _   =>
        val (remainingInput, tok) = scanOne(input)
        scanAll(remainingInput, tok :: accum)
    }
    scanAll(input)
  }

  /* The scanner accepts ints and registers of any size, but we only want to accept sufficiently small ones */
  def checkRange(t: Token): Unit = {
    try {
      t match {
        case Token("INT",x)    =>
          if (x.toLong > 4294967295l || x.toLong < -2147483648l) throw NumberFormatException
        //case Token("HEXINT",_) =>
          //if (x.toLong > 4294967295l) sys.error(s"ERROR: Hexint out of range: ${t.lexeme}")
        case _                   => Unit
      }
    } catch {
      case _: NumberFormatException => sys.error(s"ERROR: Integer out of range: ${t.lexeme}")
    }
  }

  def scan(input: String): Seq[Token] = {
    val tokens = simplifiedMaximalMunch(new WLP4DFA, input.toList).filter(WLP4.tokens contains _.kind)
    tokens.foreach(checkRange)
    tokens
  }

  def main(args: Array[String]): Unit = {
    try {
      val tokenLines = io.Source.stdin.getLines.map(scan).toSeq
      tokenLines.foreach(println)
    } catch {
      case e:Exception =>
        System.err.println("[ERROR] " + e.getMessage)
    }
  }
}
