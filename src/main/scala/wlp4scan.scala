object WLP4Scan {

  object WLP4 {

    val tokens: Set[String] = Set("ID", "NUM", "LPAREN", "RPAREN","LBRACE","RBRACE","RETURN","IF",
      "ELSE","WHILE","PRINTLN","WAIN","BECOMES","INT","EQ","NE","LT","GT","LE","GE","PLUS","MINUS",
      "STAR","SLASH","PCT","COMMA","SEMI","NEW","DELETE","LBRACK","RBRACK","AMP","NULL", "ZERO")

    val whiteSpace: Set[String] = Set("SPACE", "TAB", "NEWLINE", "COMMENT", "WHITESPACE")
  }

  trait DFA {
    /* Scala tip: single-argument member functions can be written in infix style without . or ()s
     * Don't use this if it hurts readability.
     */

    def states: Set[String]
    def alphabet: Set[Char]
    def transition: PartialFunction[(String, Char), String]
    def start: String
    def accepting: Set[String]

    /* This is unused elsewhere, but exists for completeness' sake.
     * It is extremely similar to scanOne in simplifiedMaximalMunch below, except it runs until
     * input is exhausted in all cases.
     */
    def recognize(input: Seq[Char], state: String = start): Boolean =
      if (input.isEmpty) accepting contains state
      else if (transition.isDefinedAt((state,input.head))) recognize(input.tail,transition((state,input.head)))
      else false

    /* recognize can also be defined in terms of abstract list functions, for example:
     * input.foldLeft(Some(start): Option[String])((st, ch) => st.flatMap(x => transition.lift((x, ch)))) match {
     *   case Some(state) => accepting contains state
     *   case None        => false
     * }
     */
  }

  case class Token(kind: String, lexeme: String) {

    // TODO : Init


    def ==(operand: Token): Boolean = {
      // TODO : Implement method.
      false
    }

    def print(): Unit = {
      println(kind + " " + lexeme)
    }
  }

  class WLP4DFA extends DFA {
    val alphabet: Set[Char] = ".;$,()".toSet ++ ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')
    val accepting: Set[String] = WLP4.tokens ++ WLP4.whiteSpace
    val states: Set[String] = accepting ++ Set("MINUS", "START", "INVALID")
    val start: String = "START"

    val syms: Seq[Char] = Seq[Char]('=', '<', '>')

    require(accepting subsetOf states)
    require(states contains start)

    override def transition: PartialFunction[(String, Char), String] = {
      case ("START",x)      if x.isLetter               => "ID"
      case ("START", '0')                               => "ZERO"
      case ("ZERO", x)      if x.isLetterOrDigit        => "INVALID"
      case ("ID",x)         if x.isLetterOrDigit        => "ID"
      case ("START", x)     if x.isDigit                => "NUM"
      case ("NUM", x)       if x.isDigit                => "NUM"
      case ("NUM", x)       if x.isLetter               => "INVALID"
      case ("START",'-')                                => "MINUS"
      case ("START",'+')                                => "PLUS"
      case ("START", '}')                               => "RBRACE"
      case ("START", '{')                               => "LBRACE"
      case ("START", ']')                               => "RBRACK"
      case ("START", '[')                               => "LBRACK"
      case ("START", ')')                               => "RPAREN"
      case ("START", '(')                               => "LPAREN"
      case ("START", '=')                               => "BECOMES"
      case ("BECOMES", '=')                             => "EQ"
      case ("START", '!')                               => "BANG"
      case ("BANG", '=')                                => "NE"
      case ("START", '<')                               => "LT"
      case ("LT", '=')                                  => "LE"
      case ("START", '>')                               => "GT"
      case ("GT", '=')                                  => "GE"

      case ("LT", x)        if syms.contains(x)         => "INVALID"
      case ("EQ", x)        if syms.contains(x)         => "INVALID"
      case ("NE", x)        if syms.contains(x)         => "INVALID"
      case ("LT", x)        if syms.contains(x)         => "INVALID"
      case ("LE", x)        if syms.contains(x)         => "INVALID"
      case ("GT", x)        if syms.contains(x)         => "INVALID"
      case ("GE", x)        if syms.contains(x)         => "INVALID"



      case ("START", '*')                               => "STAR"
      case ("START", '%')                               => "PCT"
      case ("START", '&')                               => "AMP"
      case ("START",',')                                => "COMMA"
      case ("START",';')                                => "SEMI"
      case ("START",'/')                                => "SLASH"
      case ("SLASH", '/')                               => "COMMENT"
      case ("COMMENT", x)   if x != '\n'                => "COMMENT"
      case ("START",x)      if x.isWhitespace           => "WHITESPACE"
      case ("WHITESPACE",x) if x.isWhitespace           => "WHITESPACE"
    }
  }

  val wlp4dfa: DFA = new WLP4DFA()

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
        case Token("NUM",x)    =>
          if (x.toLong > 2147483647l || x.toLong < -2147483648l) throw new NumberFormatException
        //case Token("HEXINT",_) =>
          //if (x.toLong > 4294967295l) sys.error(s"ERROR: Hexint out of range: ${t.lexeme}")
        case _                   => Unit
      }
    } catch {
      case _: NumberFormatException => sys.error(s"ERROR: Integer out of range: ${t.lexeme}")
    }
  }

  def scan(input: String): Seq[Token] = {
    val tokens = simplifiedMaximalMunch(wlp4dfa, input.toList).filter(WLP4.tokens contains _.kind).map {
      case Token("ID", "int")                                                            =>
        Token("INT","int")
      case Token("ID", "wain")                                                           =>
        Token("WAIN","wain")
      case Token("ID", "return")                                                         =>
        Token("RETURN","return")
      case Token("ID", "if")                                                             =>
        Token("IF", "if")
      case Token("ID", "else")                                                           =>
        Token("ELSE", "else")
      case Token("ID", "while")                                                          =>
        Token("WHILE", "while")
      case Token("ID", "println")                                                        =>
        Token("PRINTLN", "println")
      case Token("ID", "new")                                                            =>
        Token("NEW", "new")
      case Token("ID", "delete")                                                         =>
        Token("DELETE", "delete")
      case Token("ID", "NULL")                                                           =>
        Token("NULL", "NULL")
      case Token("ZERO", "0")                                                            =>
        Token("NUM", "0")
      case t                                                                             =>
        t
      /* Scala tip: For lambda functions which only use their argument in one place, you can
       * usually use an _ rather than defining a variable explicitly. The filter below is
       * equivalent to filter(x => asmTokens contains x.kind)
       */
    }.filter(WLP4.tokens contains _.kind) /* Remove whitespace and comments */
    tokens.foreach(checkRange)

    tokens
  }

  def main(args: Array[String]): Unit = {
    try {
      val tokenLines = io.Source.stdin.getLines.map(scan).toSeq
      for (tokenLine <- tokenLines) {
        for (token <- tokenLine) {
          token.print()
        }
      }
    } catch {
      case e:Exception =>
        System.err.println("[ERROR] " + e.getMessage)
    }
  }
}
