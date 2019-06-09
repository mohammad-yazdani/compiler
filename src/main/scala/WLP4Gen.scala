object WLP4Gen {

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

      def out(seq: Seq[String]): Seq[String] = {
        seq.+:(kind + " " + lexeme)
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

    def run(): Seq[String] = {
      try {
        var output: Seq[String] = Seq.empty
        val tokenLines = io.Source.stdin.getLines.map(scan).toSeq
        for (tokenLine <- tokenLines) {
          for (token <- tokenLine) {
            output = token.out(output)
          }
        }
        output
      } catch {
        case e:Exception =>
          System.err.println("[ERROR] " + e.getMessage)
          null
      }
    }
  }

  class Transition (rawProduction: String) {
    val view: String = rawProduction
    val symbols: Seq[String] = rawProduction.split(" +").toSeq
    val LHS: String = symbols.head
    val RHS: Seq[String] = symbols.tail
  }

  class Stack[A] {
    var stack: Seq[A] = Seq.empty

    def clear(): Unit = {
      this.stack = Seq.empty
    }

    def push(item: A): Unit = {
      this.stack = this.stack.+:(item)
    }

    def pop(): A = {
      val temp: A = this.stack.head
      this.stack = this.stack.tail
      temp
    }

    def top(): A = {
      if (stack.isEmpty) throw new Exception("empty")
      this.stack.head
    }

    def reverse(): Unit = {
      this.stack = this.stack.reverse
    }

    def add(item: A): Unit = {
      this.stack = this.stack.:+(item)
    }
  }

  class Action(action: String) {
    val name: String = action
    val func: (String, Int) => Unit = action match {
      case "shift" => shift
      case "reduce" => reduce
      case t => throw new Exception("Action " + t + " not found")
    }

    def exec(sym: String, in: Int): Unit = {
      this.func(sym, in)
    }
  }

  class State {

    var rules: Map[String, (Action, Int)] = Map.empty

    def addRule(in: String, action: Action, next: Int): Unit = {
      if (!nonTerminals.contains(in) && !terminals.contains(in)) throw new Exception("Symbol " + in + " is not recognized.")
      if (next >= stateCount || next < 0) throw new Exception("State index out of range.")
      if (action.name == "reduce") {
        if (!terminals.contains(in)) throw new Exception("Non-terminal before reduce.")
        if (next >= transitions.length) throw new Exception("Rule index out of range.")
      }
      this.rules += (in -> (action, next))
    }

    def move(state: String): Unit = {
      val LHS: (Action, Int) = this.rules(state)
      LHS._1.exec(state, LHS._2)
    }
  }

  class Node[A](nvalue: String, nview: String) {
    val value: String = nvalue
    val view: String = nview

    var kind: String = _

    var children: Seq[Node[A]] = Seq.empty

    def addChild(child: Node[A]): Unit = {
      this.children = this.children.:+(child)
    }

    def print(carry: String = "--"): Unit = {
      if (this.view == null) {
        println(value + " " + readTerms(value).pop())
      } else {
        println(carry + this.view)
      }

      var arrangedRHS: Seq[Node[A]] = this.children

      var nonTermIndexes: Seq[Int] = Seq.empty
      for (word <- arrangedRHS) {
        if (nonTerminals.contains(word.value)) {
          nonTermIndexes = nonTermIndexes.:+(arrangedRHS.indexOf(word))
        }
      }

      val half: Int = nonTermIndexes.length / 2
      var offset: Int = nonTermIndexes.length - 1
      for (i <- 0 until half) {
        val right: Node[A] = arrangedRHS(nonTermIndexes(i))
        val left: Node[A] = arrangedRHS(nonTermIndexes(offset))
        arrangedRHS = arrangedRHS.updated(nonTermIndexes(i), left)
        arrangedRHS = arrangedRHS.updated(nonTermIndexes(offset), right)
        offset -= 1
      }

      arrangedRHS.foreach(child => child.print(carry))
    }

    def symPrint(carry: String = ""): Unit = {
      println(carry + this.value + "[" + this.kind + "]")
      this.children.foreach(child => child.symPrint(carry + "--"))
    }
  }


  class Symbol(var kind: String, var name: String) {
    var scope: Seq[Symbol] = Seq.empty

    private var parent: Symbol = _
    this.name = name
    this.kind = kind

    var address: Int = 1

    def add(sym: Symbol): Unit = {
      sym.setParent(this)
      this.scope = this.scope :+ sym
    }

    def get(name: String): Symbol = {
      val temp: Seq[Symbol] = this.scope.find(sym => sym.name == name).toSeq
      if (temp.nonEmpty) temp.head
      else null
    }

    def getType(id: String): String = {
      if (emit(id)) {
        this.scope.find(sym => sym.name == id).toSeq.head.kind
      }
      else if (this.name == id) this.kind
      else null
    }

    def setParent(par: Symbol): Unit = {
      this.parent = par
    }

    def emit(id: String): Boolean = {
      this.scope.exists(sym => sym.name == id)
    }

    def broadcast(id: String): Boolean = {
      if (!this.emit(id)) {
        if (this.parent != null) this.parent.broadcast(id)
        else false
      }
      else true
    }

    def print(): Unit = {
      System.err.println(this.name + " " + this.kind)
      this.scope.foreach(sym => sym.print())
    }
  }

  class Procedure(name: String, kind: String = "INT") extends Symbol(kind, name) {
    var params: Symbol = new Symbol(null, null)

    def addToParams(param: Symbol): Unit = {
      this.params.add(param)
    }

    override def getType(id: String): String = {
      if (emit(id)) {
        if (this.params.emit(id))
          this.params.scope.find(sym => sym.name == id).toSeq.head.kind
        else
          this.scope.find(sym => sym.name == id).toSeq.head.kind
      }
      else if (this.name == id) this.kind
      else if (this.broadcast(id)) symbolTable.getType(id)
      else null
    }

    override def print(): Unit = {
      System.err.print(this.name)
      this.params.scope.foreach(sym => {
        System.err.print(" " + sym.kind)
      })
      System.err.println()
      this.params.scope.foreach(sym => sym.print())
      this.scope.foreach(sym => sym.print())
    }

    override def emit(id: String): Boolean = {
      this.scope.exists(sym => sym.name == id) ||
      this.params.scope.exists(sym => sym.name == id)
    }

    override def get(name: String): Symbol = {
      val result: Symbol = super.get(name)
      if (result == null) this.params.get(name)
      else result
    }
  }

  object SymbolTable extends Symbol(null, null) {
    private var table: Seq[Symbol] = Seq.empty

    override def add(sym: Symbol): Unit = {
      sym.setParent(this)
      table = table :+ sym
      scope = scope :+ sym
    }

    override def emit(id: String): Boolean = {
      this.table.exists(sym => sym.name == id)
    }

    override def print(): Unit = {
      this.table.foreach(sym => {
        sym.print()
        System.err.println()
      })
    }
  }

  val symbolTable: Symbol = SymbolTable

  val terminals: Set[String] = Set("AMP", "BECOMES", "BOF", "COMMA", "DELETE", "ELSE", "EOF",
    "EQ", "GE", "GT", "ID", "IF", "INT", "LBRACE", "LBRACK", "LE", "LPAREN", "LT", "MINUS",
    "NE", "NEW", "NULL", "NUM", "PCT", "PLUS", "PRINTLN", "RBRACE", "RBRACK", "RETURN", "RPAREN",
    "SEMI", "SLASH", "STAR", "WAIN", "WHILE")
  val nonTerminals: Set[String] = Set("start", "dcl", "dcls", "expr", "factor", "lvalue", "procedure",
    "procedures", "main", "params", "paramlist", "statement", "statements", "term", "test", "type", "arglist", "start")
  val start: String = "start"
  var transitions: Seq[Transition] = Seq.empty
  val transitionsRaw: Seq[String] = Seq("start BOF procedures EOF", "procedures procedure procedures", "procedures main", "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE", "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE", "params", "params paramlist", "paramlist dcl", "paramlist dcl COMMA paramlist", "type INT", "type INT STAR", "dcls", "dcls dcls dcl BECOMES NUM SEMI", "dcls dcls dcl BECOMES NULL SEMI", "dcl type ID", "statements", "statements statements statement", "statement lvalue BECOMES expr SEMI", "statement IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE", "statement WHILE LPAREN test RPAREN LBRACE statements RBRACE", "statement PRINTLN LPAREN expr RPAREN SEMI", "statement DELETE LBRACK RBRACK expr SEMI", "test expr EQ expr", "test expr NE expr", "test expr LT expr", "test expr LE expr", "test expr GE expr", "test expr GT expr", "expr term", "expr expr PLUS term", "expr expr MINUS term", "term factor", "term term STAR factor", "term term SLASH factor", "term term PCT factor", "factor ID", "factor NUM", "factor NULL", "factor LPAREN expr RPAREN", "factor AMP lvalue", "factor STAR factor", "factor NEW INT LBRACK expr RBRACK", "factor ID LPAREN RPAREN", "factor ID LPAREN arglist RPAREN", "arglist expr", "arglist expr COMMA arglist", "lvalue ID", "lvalue STAR factor", "lvalue LPAREN lvalue RPAREN")
  transitionsRaw.foreach(production => this.transitions = this.transitions.:+(new Transition(production)))

  val types: Set[String] = Set("INT", "INT STAR", null)

  //var unread: Iterator[String] = Iterator("BOF BOF")
  //var unread: Iterator[String] = io.Source.stdin.getLines()
  var unread: Iterator[String] = _
  //this.unread = this.unread.++(Iterator("EOF EOF"))

  var readTerms: Map[String, Stack[String]] = Map.empty
  var readTermsTemp: Map[String, Stack[String]] = Map.empty

  var words: Iterator[String] = Iterator.empty
  var ahead: String = _
  var symbolStack: Stack[String] = new Stack[String]
  var states: Array[State] = Array.fill[State](132)(null)
  val stateCount: Int = 863

  var stateStack: Stack[Int] = new Stack[Int]

  var output: Stack[Transition] = new Stack[Transition]

  var position: Int = _
  var haveRead: Int = 0

  var inputCarry: Boolean = false

  var carry: Seq[String] = Seq.empty
  var parsed: Seq[String] = Seq.empty

  val rules: Iterator[String] = Iterator("72 RPAREN reduce 45", "112 paramlist shift 1", "85 LPAREN shift 2", "40 factor shift 3", "3 EQ reduce 47", "74 EQ reduce 46", "21 INT shift 4", "100 EQ reduce 48", "106 STAR shift 5", "86 STAR shift 5", "88 STAR shift 5", "95 SEMI shift 6", "51 SEMI shift 7", "118 NULL shift 8", "128 STAR reduce 11", "2 NULL shift 8", "108 GT shift 9", "57 RETURN reduce 16", "6 INT reduce 13", "7 INT reduce 12", "19 factor shift 10", "71 factor shift 10", "14 term shift 11", "44 term shift 11", "17 term shift 11", "25 term shift 11", "13 term shift 11", "9 term shift 11", "117 BECOMES reduce 14", "94 LPAREN reduce 11", "100 GT reduce 48", "3 GT reduce 47", "74 GT reduce 46", "85 expr shift 12", "108 GE shift 13", "74 GE reduce 46", "3 GE reduce 47", "100 GE reduce 48", "128 PRINTLN reduce 11", "108 EQ shift 14", "2 STAR shift 5", "118 STAR shift 5", "106 NEW shift 15", "86 NEW shift 15", "88 NEW shift 15", "123 RPAREN shift 16", "108 LT shift 17", "23 procedure shift 18", "70 term shift 11", "31 PLUS shift 19", "32 PLUS shift 19", "33 PLUS shift 19", "34 PLUS shift 19", "35 PLUS shift 19", "36 PLUS shift 19", "94 WHILE reduce 11", "29 COMMA shift 20", "94 dcls shift 21", "70 ID shift 22", "0 BOF shift 23", "5 NULL shift 8", "56 LPAREN shift 24", "108 LE shift 25", "101 COMMA shift 26", "121 SEMI shift 27", "83 DELETE reduce 18", "59 RETURN reduce 15", "57 WHILE reduce 16", "18 main shift 28", "82 DELETE reduce 19", "127 STAR reduce 39", "102 STAR reduce 40", "74 LE reduce 46", "30 STAR reduce 36", "8 STAR reduce 37", "112 dcl shift 29", "90 STAR reduce 43", "93 STAR reduce 42", "97 STAR reduce 38", "116 STAR reduce 41", "73 NUM shift 30", "14 expr shift 31", "44 expr shift 32", "17 expr shift 33", "25 expr shift 34", "13 expr shift 35", "9 expr shift 36", "37 EOF reduce 4", "87 RBRACE shift 37", "26 INT shift 4", "24 INT shift 4", "5 STAR shift 5", "59 STAR reduce 15", "130 DELETE reduce 17", "68 DELETE reduce 20", "131 DELETE reduce 21", "59 WHILE reduce 15", "11 LT reduce 28", "61 LT reduce 30", "60 LT reduce 29", "129 LPAREN shift 38", "121 PLUS shift 19", "46 NEW shift 15", "52 NEW shift 15", "66 NEW shift 15", "4 ID reduce 9", "38 LPAREN shift 38", "85 STAR shift 5", "18 procedures shift 39", "85 NUM shift 30", "40 NULL shift 8", "61 RPAREN reduce 30", "60 RPAREN reduce 29", "11 RPAREN reduce 28", "59 ID reduce 15", "59 IF reduce 15", "57 RBRACE reduce 16", "61 NE reduce 30", "60 NE reduce 29", "11 NE reduce 28", "77 IF reduce 15", "76 IF reduce 15", "115 STAR shift 40", "78 ID reduce 15", "78 IF reduce 15", "74 RBRACK reduce 46", "3 RBRACK reduce 47", "100 RBRACK reduce 48", "128 LPAREN reduce 11", "77 ID reduce 15", "76 ID reduce 15", "15 INT shift 41", "113 STAR shift 40", "114 STAR shift 40", "20 type shift 42", "22 RPAREN reduce 35", "24 type shift 42", "26 type shift 42", "129 WHILE shift 43", "108 NE shift 44", "11 LE reduce 28", "70 factor shift 10", "105 NEW shift 15", "104 NEW shift 15", "80 term shift 11", "80 expr shift 12", "113 LPAREN shift 38", "114 LPAREN shift 38", "117 RPAREN reduce 14", "115 LPAREN shift 38", "61 LE reduce 30", "60 LE reduce 29", "63 BECOMES shift 45", "90 PCT reduce 43", "116 PCT reduce 41", "30 PCT reduce 36", "8 PCT reduce 37", "127 PCT reduce 39", "102 PCT reduce 40", "93 PCT reduce 42", "97 PCT reduce 38", "47 MINUS reduce 32", "48 MINUS reduce 33", "49 MINUS reduce 34", "10 MINUS reduce 31", "10 STAR reduce 31", "11 STAR shift 46", "61 STAR shift 46", "60 STAR shift 46", "70 LPAREN shift 2", "46 factor shift 47", "52 factor shift 48", "66 factor shift 49", "14 STAR shift 5", "44 STAR shift 5", "17 STAR shift 5", "25 STAR shift 5", "13 STAR shift 5", "9 STAR shift 5", "80 arglist shift 50", "12 RPAREN reduce 44", "19 NEW shift 15", "71 NEW shift 15", "45 NUM shift 51", "61 GE reduce 30", "60 GE reduce 29", "11 GE reduce 28", "10 RBRACK reduce 31", "47 RBRACK reduce 32", "48 RBRACK reduce 33", "49 RBRACK reduce 34", "90 COMMA reduce 43", "116 COMMA reduce 41", "61 GT reduce 30", "60 GT reduce 29", "11 SLASH shift 52", "11 GT reduce 28", "61 SLASH shift 52", "60 SLASH shift 52", "30 COMMA reduce 36", "8 COMMA reduce 37", "127 COMMA reduce 39", "102 COMMA reduce 40", "47 STAR reduce 32", "48 STAR reduce 33", "49 STAR reduce 34", "80 ID shift 22", "93 COMMA reduce 42", "97 COMMA reduce 38", "70 NULL shift 8", "59 RBRACE reduce 15", "46 AMP shift 53", "52 AMP shift 53", "66 AMP shift 53", "23 main shift 28", "94 ID reduce 11", "78 RBRACE reduce 15", "94 IF reduce 11", "94 PRINTLN reduce 11", "23 INT shift 54", "5 ID shift 22", "46 STAR shift 5", "52 STAR shift 5", "66 STAR shift 5", "113 DELETE shift 55", "114 DELETE shift 55", "116 SLASH reduce 41", "129 STAR shift 40", "90 SLASH reduce 43", "112 RPAREN reduce 5", "54 WAIN shift 56", "127 SLASH reduce 39", "102 SLASH reduce 40", "93 SLASH reduce 42", "97 SLASH reduce 38", "30 SLASH reduce 36", "8 SLASH reduce 37", "19 STAR shift 5", "71 STAR shift 5", "118 NUM shift 30", "109 statement shift 57", "2 NUM shift 30", "73 LPAREN shift 2", "53 LPAREN shift 38", "47 COMMA reduce 32", "48 COMMA reduce 33", "49 COMMA reduce 34", "14 factor shift 10", "44 factor shift 10", "17 factor shift 10", "25 factor shift 10", "13 factor shift 10", "9 factor shift 10", "10 COMMA reduce 31", "100 STAR reduce 48", "3 STAR reduce 47", "74 STAR reduce 46", "115 DELETE shift 55", "109 PRINTLN shift 58", "78 WHILE reduce 15", "77 RBRACE reduce 15", "76 RBRACE reduce 15", "113 statement shift 57", "114 statement shift 57", "22 PLUS reduce 35", "115 statement shift 57", "105 AMP shift 53", "104 AMP shift 53", "77 WHILE reduce 15", "76 WHILE reduce 15", "128 dcls shift 59", "118 term shift 11", "2 term shift 11", "19 term shift 60", "71 term shift 61", "128 WHILE reduce 11", "19 LPAREN shift 2", "71 LPAREN shift 2", "128 RETURN reduce 11", "57 ID reduce 16", "57 IF reduce 16", "23 procedures shift 62", "2 LPAREN shift 2", "118 LPAREN shift 2", "21 dcl shift 63", "19 NULL shift 8", "71 NULL shift 8", "78 PRINTLN reduce 15", "77 PRINTLN reduce 15", "76 PRINTLN reduce 15", "2 expr shift 64", "118 expr shift 65", "11 PCT shift 66", "21 ID reduce 15", "21 IF reduce 15", "6 LPAREN reduce 13", "7 LPAREN reduce 12", "113 lvalue shift 67", "114 lvalue shift 67", "14 NULL shift 8", "44 NULL shift 8", "17 NULL shift 8", "25 NULL shift 8", "13 NULL shift 8", "9 NULL shift 8", "118 AMP shift 53", "115 lvalue shift 67", "2 AMP shift 53", "16 SEMI shift 68", "80 factor shift 10", "94 INT reduce 11", "5 NEW shift 15", "73 STAR shift 5", "57 PRINTLN reduce 16", "80 NEW shift 15", "74 LT reduce 46", "3 LT reduce 47", "46 NUM shift 30", "52 NUM shift 30", "66 NUM shift 30", "10 PCT reduce 31", "3 LE reduce 47", "100 LE reduce 48", "85 NEW shift 15", "21 WHILE reduce 15", "47 PCT reduce 32", "48 PCT reduce 33", "49 PCT reduce 34", "100 LT reduce 48", "21 RBRACE reduce 15", "22 COMMA reduce 35", "129 PRINTLN shift 58", "22 SEMI reduce 35", "100 NE reduce 48", "3 NE reduce 47", "74 NE reduce 46", "40 AMP shift 53", "14 ID shift 22", "44 ID shift 22", "17 ID shift 22", "25 ID shift 22", "13 ID shift 22", "9 ID shift 22", "94 RETURN reduce 11", "46 NULL shift 8", "52 NULL shift 8", "66 NULL shift 8", "19 AMP shift 53", "71 AMP shift 53", "118 ID shift 22", "2 ID shift 22", "54 ID shift 69", "80 NULL shift 8", "129 RETURN shift 70", "61 PLUS reduce 30", "60 PLUS reduce 29", "11 PLUS reduce 28", "59 DELETE reduce 15", "22 PCT reduce 35", "31 MINUS shift 71", "32 MINUS shift 71", "33 MINUS shift 71", "34 MINUS shift 71", "35 MINUS shift 71", "36 MINUS shift 71", "105 NUM shift 30", "104 NUM shift 30", "85 arglist shift 72", "22 RBRACK reduce 35", "6 DELETE reduce 13", "7 DELETE reduce 12", "109 RETURN shift 73", "88 LPAREN shift 2", "12 PLUS shift 19", "53 ID shift 74", "105 LPAREN shift 2", "104 LPAREN shift 2", "106 LPAREN shift 2", "86 LPAREN shift 2", "22 MINUS reduce 35", "90 BECOMES reduce 43", "93 BECOMES reduce 42", "97 BECOMES reduce 38", "127 BECOMES reduce 39", "102 BECOMES reduce 40", "30 BECOMES reduce 36", "8 BECOMES reduce 37", "70 NUM shift 30", "116 BECOMES reduce 41", "19 NUM shift 30", "71 NUM shift 30", "73 expr shift 75", "94 STAR reduce 11", "126 LBRACE shift 76", "125 LBRACE shift 77", "100 SLASH reduce 48", "3 SLASH reduce 47", "96 LBRACE shift 78", "74 SLASH reduce 46", "55 LBRACK shift 79", "40 STAR shift 5", "128 INT reduce 11", "2 NEW shift 15", "22 LPAREN shift 80", "127 NE reduce 39", "102 NE reduce 40", "93 NE reduce 42", "97 NE reduce 38", "90 NE reduce 43", "116 NE reduce 41", "116 MINUS reduce 41", "38 STAR shift 40", "78 RETURN reduce 15", "112 INT shift 4", "30 NE reduce 36", "8 NE reduce 37", "30 MINUS reduce 36", "8 MINUS reduce 37", "118 NEW shift 15", "127 MINUS reduce 39", "102 MINUS reduce 40", "93 MINUS reduce 42", "97 MINUS reduce 38", "90 MINUS reduce 43", "77 RETURN reduce 15", "76 RETURN reduce 15", "39 EOF reduce 1", "6 WHILE reduce 13", "7 WHILE reduce 12", "28 EOF reduce 2", "59 PRINTLN reduce 15", "113 RBRACE shift 81", "114 RBRACE shift 82", "127 RBRACK reduce 39", "102 RBRACK reduce 40", "93 RBRACK reduce 42", "97 RBRACK reduce 38", "109 lvalue shift 67", "90 RBRACK reduce 43", "116 RBRACK reduce 41", "115 RBRACE shift 83", "30 RBRACK reduce 36", "8 RBRACK reduce 37", "21 PRINTLN reduce 15", "14 LPAREN shift 2", "44 LPAREN shift 2", "17 LPAREN shift 2", "25 LPAREN shift 2", "13 LPAREN shift 2", "9 LPAREN shift 2", "112 params shift 84", "12 COMMA shift 85", "75 MINUS shift 71", "22 SLASH reduce 35", "103 RPAREN reduce 8", "5 NUM shift 30", "67 BECOMES shift 86", "11 RBRACK reduce 28", "61 COMMA reduce 30", "60 COMMA reduce 29", "61 RBRACK reduce 30", "60 RBRACK reduce 29", "11 COMMA reduce 28", "74 PCT reduce 46", "3 PCT reduce 47", "100 PCT reduce 48", "75 PLUS shift 19", "5 LPAREN shift 2", "112 type shift 42", "85 ID shift 22", "3 BECOMES reduce 47", "74 BECOMES reduce 46", "100 BECOMES reduce 48", "85 term shift 11", "116 SEMI reduce 41", "64 MINUS shift 71", "93 SEMI reduce 42", "97 SEMI reduce 38", "65 MINUS shift 71", "90 SEMI reduce 43", "80 LPAREN shift 2", "75 SEMI shift 87", "130 RBRACE reduce 17", "68 RBRACE reduce 20", "131 RBRACE reduce 21", "14 AMP shift 53", "44 AMP shift 53", "17 AMP shift 53", "25 AMP shift 53", "13 AMP shift 53", "9 AMP shift 53", "82 RBRACE reduce 19", "79 RBRACK shift 88", "127 SEMI reduce 39", "102 SEMI reduce 40", "30 SEMI reduce 36", "8 SEMI reduce 37", "83 RBRACE reduce 18", "68 PRINTLN reduce 20", "131 PRINTLN reduce 21", "130 PRINTLN reduce 17", "47 PLUS reduce 32", "48 PLUS reduce 33", "49 PLUS reduce 34", "82 PRINTLN reduce 19", "10 PLUS reduce 31", "109 WHILE shift 43", "83 PRINTLN reduce 18", "65 PLUS shift 19", "64 PLUS shift 19", "77 STAR reduce 15", "76 STAR reduce 15", "10 SEMI reduce 31", "78 STAR reduce 15", "73 NULL shift 8", "46 LPAREN shift 2", "52 LPAREN shift 2", "66 LPAREN shift 2", "4 STAR shift 89", "47 SEMI reduce 32", "48 SEMI reduce 33", "49 SEMI reduce 34", "116 RPAREN reduce 41", "40 NUM shift 30", "30 RPAREN reduce 36", "8 RPAREN reduce 37", "127 RPAREN reduce 39", "102 RPAREN reduce 40", "50 RPAREN shift 90", "93 RPAREN reduce 42", "97 RPAREN reduce 38", "105 NULL shift 8", "104 NULL shift 8", "90 RPAREN reduce 43", "109 DELETE shift 55", "18 procedure shift 18", "100 PLUS reduce 48", "74 PLUS reduce 46", "3 PLUS reduce 47", "85 factor shift 10", "10 NE reduce 31", "47 NE reduce 32", "48 NE reduce 33", "49 NE reduce 34", "94 DELETE reduce 11", "116 EQ reduce 41", "100 COMMA reduce 48", "30 GT reduce 36", "8 GT reduce 37", "3 COMMA reduce 47", "93 GT reduce 42", "97 GT reduce 38", "127 GT reduce 39", "102 GT reduce 40", "1 RPAREN reduce 6", "74 COMMA reduce 46", "27 RBRACE shift 91", "90 GE reduce 43", "93 GE reduce 42", "97 GE reduce 38", "116 GE reduce 41", "83 WHILE reduce 18", "30 GE reduce 36", "8 GE reduce 37", "127 GE reduce 39", "102 GE reduce 40", "21 LPAREN reduce 15", "6 STAR reduce 13", "7 STAR reduce 12", "82 WHILE reduce 19", "68 WHILE reduce 20", "131 WHILE reduce 21", "130 WHILE reduce 17", "61 PCT shift 66", "60 PCT shift 66", "80 NUM shift 30", "20 dcl shift 29", "99 RPAREN shift 92", "128 ID reduce 11", "57 LPAREN reduce 16", "80 RPAREN shift 93", "92 LBRACE shift 94", "14 NUM shift 30", "44 NUM shift 30", "17 NUM shift 30", "25 NUM shift 30", "13 NUM shift 30", "9 NUM shift 30", "90 GT reduce 43", "116 GT reduce 41", "45 NULL shift 95", "128 IF reduce 11", "81 ELSE shift 96", "57 DELETE reduce 16", "64 RPAREN shift 97", "73 AMP shift 53", "62 EOF shift 98", "21 STAR reduce 15", "118 factor shift 10", "105 factor shift 10", "104 factor shift 10", "2 factor shift 10", "29 RPAREN reduce 7", "100 RPAREN reduce 48", "105 ID shift 22", "104 ID shift 22", "82 LPAREN reduce 19", "20 INT shift 4", "88 factor shift 10", "130 LPAREN reduce 17", "106 factor shift 10", "86 factor shift 10", "68 LPAREN reduce 20", "131 LPAREN reduce 21", "26 dcl shift 99", "19 ID shift 22", "71 ID shift 22", "74 RPAREN reduce 46", "111 RPAREN shift 100", "3 RPAREN reduce 47", "83 LPAREN reduce 18", "24 dcl shift 101", "5 factor shift 102", "20 paramlist shift 103", "107 LPAREN shift 104", "43 LPAREN shift 105", "58 LPAREN shift 106", "12 MINUS shift 71", "11 MINUS reduce 28", "61 MINUS reduce 30", "60 MINUS reduce 29", "59 INT shift 4", "93 LT reduce 42", "97 LT reduce 38", "90 LT reduce 43", "30 LT reduce 36", "8 LT reduce 37", "127 LT reduce 39", "102 LT reduce 40", "91 INT reduce 3", "116 LT reduce 41", "93 LE reduce 42", "97 LE reduce 38", "108 PLUS shift 19", "127 LE reduce 39", "102 LE reduce 40", "30 LE reduce 36", "8 LE reduce 37", "129 ID shift 74", "116 LE reduce 41", "129 IF shift 107", "90 LE reduce 43", "46 ID shift 22", "52 ID shift 22", "66 ID shift 22", "70 NEW shift 15", "88 NULL shift 8", "106 NULL shift 8", "86 NULL shift 8", "10 EQ reduce 31", "47 EQ reduce 32", "48 EQ reduce 33", "49 EQ reduce 34", "80 STAR shift 5", "6 PRINTLN reduce 13", "7 PRINTLN reduce 12", "22 LT reduce 35", "59 LPAREN reduce 15", "59 type shift 42", "73 NEW shift 15", "22 LE reduce 35", "109 STAR shift 40", "105 expr shift 108", "104 expr shift 108", "93 PLUS reduce 42", "97 PLUS reduce 38", "90 PLUS reduce 43", "116 PLUS reduce 41", "30 PLUS reduce 36", "8 PLUS reduce 37", "127 PLUS reduce 39", "102 PLUS reduce 40", "10 GT reduce 31", "47 GT reduce 32", "48 GT reduce 33", "49 GT reduce 34", "109 IF shift 107", "109 ID shift 74", "80 AMP shift 53", "108 MINUS shift 71", "10 GE reduce 31", "47 GE reduce 32", "48 GE reduce 33", "49 GE reduce 34", "21 statements shift 109", "109 LPAREN shift 38", "59 dcl shift 63", "128 DELETE reduce 11", "40 ID shift 22", "84 RPAREN shift 110", "88 NUM shift 30", "106 NUM shift 30", "86 NUM shift 30", "40 LPAREN shift 2", "38 lvalue shift 111", "22 BECOMES reduce 35", "40 NEW shift 15", "69 LPAREN shift 112", "77 statements shift 113", "76 statements shift 114", "73 term shift 11", "78 statements shift 115", "53 STAR shift 40", "57 STAR reduce 16", "93 EQ reduce 42", "97 EQ reduce 38", "90 EQ reduce 43", "30 EQ reduce 36", "8 EQ reduce 37", "127 EQ reduce 39", "102 EQ reduce 40", "85 AMP shift 53", "47 LT reduce 32", "48 LT reduce 33", "49 LT reduce 34", "10 LT reduce 31", "117 COMMA reduce 14", "18 INT shift 54", "83 RETURN reduce 18", "10 LE reduce 31", "3 MINUS reduce 47", "129 statement shift 57", "74 MINUS reduce 46", "22 NE reduce 35", "10 RPAREN reduce 31", "100 MINUS reduce 48", "47 LE reduce 32", "48 LE reduce 33", "49 LE reduce 34", "106 term shift 11", "86 term shift 11", "130 RETURN reduce 17", "47 RPAREN reduce 32", "48 RPAREN reduce 33", "49 RPAREN reduce 34", "70 AMP shift 53", "88 term shift 11", "68 RETURN reduce 20", "131 RETURN reduce 21", "82 RETURN reduce 19", "105 term shift 11", "104 term shift 11", "82 STAR reduce 19", "22 STAR reduce 35", "83 STAR reduce 18", "77 LPAREN reduce 15", "76 LPAREN reduce 15", "65 RBRACK shift 116", "70 STAR shift 5", "68 STAR reduce 20", "131 STAR reduce 21", "78 LPAREN reduce 15", "130 STAR reduce 17", "129 DELETE shift 55", "121 MINUS shift 71", "78 DELETE reduce 15", "42 ID shift 117", "77 DELETE reduce 15", "76 DELETE reduce 15", "113 ID shift 74", "114 ID shift 74", "113 IF shift 107", "114 IF shift 107", "115 ID shift 74", "115 IF shift 107", "38 ID shift 74", "123 PLUS shift 19", "124 PLUS shift 19", "122 PLUS shift 19", "47 SLASH reduce 32", "48 SLASH reduce 33", "49 SLASH reduce 34", "10 SLASH reduce 31", "41 LBRACK shift 118", "14 NEW shift 15", "44 NEW shift 15", "17 NEW shift 15", "25 NEW shift 15", "13 NEW shift 15", "9 NEW shift 15", "105 STAR shift 5", "104 STAR shift 5", "105 test shift 119", "104 test shift 120", "5 AMP shift 53", "122 MINUS shift 71", "123 MINUS shift 71", "124 MINUS shift 71", "70 expr shift 121", "100 SEMI reduce 48", "73 ID shift 22", "3 SEMI reduce 47", "74 SEMI reduce 46", "113 WHILE shift 43", "114 WHILE shift 43", "88 ID shift 22", "106 ID shift 22", "86 ID shift 22", "22 GE reduce 35", "115 WHILE shift 43", "88 expr shift 122", "106 expr shift 123", "86 expr shift 124", "120 RPAREN shift 125", "119 RPAREN shift 126", "22 GT reduce 35", "129 lvalue shift 67", "11 SEMI reduce 28", "6 IF reduce 13", "7 IF reduce 12", "61 SEMI reduce 30", "60 SEMI reduce 29", "53 lvalue shift 127", "6 ID reduce 13", "7 ID reduce 12", "88 AMP shift 53", "89 ID reduce 10", "110 LBRACE shift 128", "106 AMP shift 53", "86 AMP shift 53", "83 IF reduce 18", "83 ID reduce 18", "82 IF reduce 19", "130 IF reduce 17", "82 ID reduce 19", "68 IF reduce 20", "131 IF reduce 21", "31 RPAREN reduce 22", "32 RPAREN reduce 23", "33 RPAREN reduce 24", "34 RPAREN reduce 25", "35 RPAREN reduce 26", "36 RPAREN reduce 27", "21 type shift 42", "6 RETURN reduce 13", "7 RETURN reduce 12", "59 statements shift 129", "68 ID reduce 20", "131 ID reduce 21", "130 ID reduce 17", "22 EQ reduce 35", "11 EQ reduce 28", "73 factor shift 10", "61 EQ reduce 30", "60 EQ reduce 29", "85 NULL shift 8", "115 PRINTLN shift 58", "124 SEMI shift 130", "122 SEMI shift 131", "21 RETURN reduce 15", "113 PRINTLN shift 58", "114 PRINTLN shift 58", "21 DELETE reduce 15")


  def fillStates(count: Int): Unit = {
    for (_ <- 0 until count) {
      val raw: Seq[String] = rules.next().split(" +").toSeq

      try {
        val key: Int = raw.head.toInt
        if (key >= stateCount || key < 0) throw new Exception("State index out of range.")
        val in: String = raw(1)
        val action: Action = new Action(raw(2))
        val next: Int = raw(3).toInt
        if (this.states(key).isInstanceOf[State]) {
          this.states(key).addRule(in, action, next)
        } else {
          val state = new State()
          state.addRule(in, action, next)
          this.states(key) = state
        }
      } catch {
        case e: Exception =>
          System.err.print(e.getMessage)
          //System.exit(1)
      }
    }
  }

  def read(): String = {
    val temp: Seq[String] = this.unread.next().split(" +").toSeq
    if (!this.readTerms.contains(temp.head)) {
      val newStack: Stack[String] = new Stack[String]
      newStack.push(temp(1))
      this.readTerms += (temp.head -> newStack)
    } else {
      this.readTerms(temp.head).push(temp(1))
    }
    temp.head
  }

  def fullRead(): String = {
    val line: String = this.unread.next()
    val temp: Seq[String] = line.split(" +").toSeq
    if (this.terminals.contains(temp.head)) {
      if (!this.readTerms.contains(temp.head)) {
        val newStack: Stack[String] = new Stack[String]
        newStack.push(temp(1))
        this.readTerms += (temp.head -> newStack)
      } else {
        this.readTerms(temp.head).push(temp(1))
      }
    }
    line
  }

  def readTok(): String = {
    if (!this.unread.hasNext && !this.words.hasNext) {
      return null
    }
    if (this.ahead != null) {
      val temp: String = this.ahead
      this.ahead = null
      temp
    } else {
      this.haveRead += 1
      if (!this.words.hasNext) {
        val in: String = read()
        val raw: Seq[String] = in.split(" +").filter(sym => sym != "").toSeq
        this.words = raw.toIterator
        this.inputCarry = true
      }
      this.words.next()
    }
  }

  def lookAhead(): String = {
    val temp: String = readTok()
    if (temp != null) this.ahead = temp
    this.ahead
  }

  def reduceCheck(state: Int, sym: String): Boolean = {
    if (this.states(state).rules.contains(sym)) {
      if (this.states(state).rules(sym)._1.name == "reduce") true
      else false
    } else false
  }

  def shift(sym: String, in: Int): Unit = {
    this.symbolStack.push(sym)
    this.stateStack.push(in)
  }

  def reduce(sym: String, in: Int): Unit = {
    var popped: Seq[String] = Seq.empty
    for (_ <- this.transitions(in).RHS.indices) {
      val justPopped: String = symbolStack.pop()
      popped = popped.+:(justPopped)
    }
    if (this.transitions(in).RHS == popped) {
      val lhs: String = this.transitions(in.toInt).LHS
      this.output.push(this.transitions(in.toInt))
      for (_ <- popped.indices) this.stateStack.pop()
      this.states(stateStack.top()).move(lhs)
    } else throw new Exception("Wrong reduce.")
  }

  def parse(): Boolean = {
    if (this.symbolStack.stack.equals(this.transitions.head.RHS.reverse)) {
      this.output.push(this.transitions.head)
      this.symbolStack.clear()
      this.stateStack.clear()
      finish()
      return false
    }
    var curr: Int = 0
    try {
      curr = this.stateStack.top()
    } catch {
      case _: Exception =>
    }
    try {
      if (this.reduceCheck(curr, lookAhead())) {
        this.states(curr).move(lookAhead())
        true
      } else {
        var in: String = this.readTok()
        if (in == null) {
          in = ahead
        }
        this.states(curr).move(state = in)
        true
      }
    } catch {
      case _: Exception =>
        error()
        false
    }
  }

  def error(): Unit = {
    System.err.println("ERROR at " + (this.haveRead - 1))
    //System.exit(1)
  }

  def finish(): Unit = {
    for (item <- this.output.stack) {
      this.parsed = this.parsed.+:(item.view)
    }
    this.parsed = this.parsed.reverse
    this.readTerms.foreach(stack => stack._2.reverse())
    this.output.clear()
  }

  def reArrangeOutput(): Unit = {
    this.output.stack.map(curr => {
      val nonTermIndexes: Seq[Int] = curr.RHS
        .filter(item => nonTerminals.contains(item)).map(item => curr.RHS.indexOf(item))
      val nonTermIndexesRev: Seq[Int] = nonTermIndexes.reverse

      val newRHS: Seq[String] = curr.RHS
      for (i <- nonTermIndexes.indices) {
        newRHS.updated(nonTermIndexes(i), curr.RHS(nonTermIndexesRev(i)))
      }
      curr
    })
  }

  def genTree(root: String): Node[String] = {
    val curr: Transition = this.output.pop()
    val currNode: Node[String] = new Node[String](curr.LHS, curr.view)

    for (word <- curr.RHS) {
      if (this.terminals.contains(word)) {
        currNode.addChild(new Node[String](word, null))
      } else {
        currNode.addChild(genTree(word))
      }
    }
    currNode
  }

  def typeResolve(kind: Seq[String]): String = {
    kind match {
      case Seq("INT") => "INT"
      case Seq("INT", "STAR") => "INT STAR"
      case t => throw new Exception("Invalid type " + t)
    }
  }

  def buildSymbolTable(node: Node[String], scope: Symbol): Unit = {
    node.value match {
      case "dcl" =>
        val kindRaw: Seq[String] = node.children.head.children.map(child => child.value)
        val kind: String = this.typeResolve(kindRaw)
        val id: String = this.readTerms("ID").pop()

        if (!this.readTermsTemp.contains("ID")) {
          val newStack: Stack[String] = new Stack[String]
          newStack.push(id)
          this.readTermsTemp += ("ID" -> newStack)
        } else {
          this.readTermsTemp("ID").push(id)
        }

        if (scope.emit(id)) throw new Exception("Duplicate declaration of variable " + id)
        scope.add(new Symbol(kind = kind, name = id))
      case "procedure" =>
        val id: String =  this.readTerms("ID").pop()

        if (!this.readTermsTemp.contains("ID")) {
          val newStack: Stack[String] = new Stack[String]
          newStack.push(id)
          this.readTermsTemp += ("ID" -> newStack)
        } else {
          this.readTermsTemp("ID").push(id)
        }

        if (scope.emit(id)) throw new Exception("Duplicate declaration of function " + id)
        val procedure: Procedure = new Procedure(id)
        scope.add(procedure)
        node.children.foreach(child => {
          if (child.value == "params") this.buildSymbolTable(child, procedure.params)
          else this.buildSymbolTable(child, procedure)
        })
      case "main" =>
        if (scope.emit("wain")) throw new Exception("Duplicate declaration of wain")
        val procedure: Procedure = new Procedure("wain")
        scope.add(procedure)
        node.children.foreach(child => {
          if (child.value == "dcl") this.buildSymbolTable(child, procedure.params)
          else this.buildSymbolTable(child, procedure)
        })
      case "factor" | "lvalue" =>
        if (node.children.head.value == "ID" && node.children.length < 2) {
          val id: String = this.readTerms("ID").pop()

          if (!this.readTermsTemp.contains("ID")) {
            val newStack: Stack[String] = new Stack[String]
            newStack.push(id)
            this.readTermsTemp += ("ID" -> newStack)
          } else {
            this.readTermsTemp("ID").push(id)
          }

          if (!scope.emit(id))
            throw new Exception("Undeclared variable " + id)
        } else if (node.children.head.value == "ID") {
          val id: String = this.readTerms("ID").pop()

          if (!this.readTermsTemp.contains("ID")) {
            val newStack: Stack[String] = new Stack[String]
            newStack.push(id)
            this.readTermsTemp += ("ID" -> newStack)
          } else {
            this.readTermsTemp("ID").push(id)
          }

          if (scope.emit(id)) throw new Exception(id + " is not a function.")
          if (!scope.broadcast(id)) throw new Exception("Undeclared function " + id)
        }
        node.children.foreach(child => this.buildSymbolTable(child, scope))
      case _ =>
        node.children.foreach(child => this.buildSymbolTable(child, scope))
    }
  }

  def handleArgs(arglist: Node[String], function: Procedure): Node[String] = {
    var argStack: Stack[String] = new Stack[String]
    argStack = this.handleArgList(function, arglist, argStack)
    for (param <- function.params.scope) {
      if (argStack.stack.isEmpty) throw new Exception("Too few arguments to function " + function.name)
      val actualKind: String = argStack.pop()
      if (param.kind != actualKind) throw new Exception("Parameter " + param.name +
        " of function " + function.name + " cannot take type " + actualKind)
    }
    if (argStack.stack.nonEmpty) throw new Exception("Too many arguments to function " + function.name)

    arglist
  }

  def handleArgList(scope: Procedure, root: Node[String], stack: Stack[String]): Stack[String] = {
    var mutableStack: Stack[String] = stack
    root.value match {
      case "expr" =>
        mutableStack.push(this.handleExpr(root, scope).kind)
        mutableStack
      case _ =>
        root.children.foreach(child => {
          mutableStack = this.handleArgList(scope, child, mutableStack)
        })
        mutableStack
    }
  }

  def evaluate(kind: String, op: String): String = {
    (kind, op) match {
      case ("INT", "AMP") => "INT STAR"
      case ("INT STAR", "STAR") => "INT"

      case ("INT", "PLUS") => "INT PLUS"
      case ("INT STAR", "PLUS") => "INT STAR PLUS"
      case ("INT PLUS", "INT") =>  "INT"
      case ("INT PLUS", "INT STAR") =>  "INT STAR"
      case ("INT STAR PLUS", "INT") =>  "INT STAR"

      case ("INT", "MINUS") => "INT MINUS"
      case ("INT STAR", "MINUS") => "INT STAR MINUS"
      case ("INT MINUS", "INT") =>  "INT"
      case ("INT STAR MINUS", "INT STAR") =>  "INT"
      case ("INT STAR MINUS", "INT") =>  "INT STAR"

      case ("INT", "STAR" | "SLASH" | "PCT") => "MULT-DIV"
      case ("MULT-DIV", "INT") => "INT"

      case (null, "NEW") => "NEW INT"
      case ("NEW INT", "LBRACK") => "NEW INT ["
      case ("NEW INT [", "INT") => "NEW INT [INT"
      case ("NEW INT [INT", "RBRACK") => "INT STAR"

      case (org, "LPAREN") => org
      case (org, "RPAREN") => org
      case (org, "arglist") => org

      case _ => throw new Exception("Operation " + op + " cannot be applied to type " + kind + ".")
    }
  }

  def resolveExpr(expr: Iterator[Node[String]]): String = {
    var exprType: String = null
    var op: String = null

    val current: Node[String] = expr.next()
    if (current.kind != null) exprType = current.kind
    else op = current.value

    while (expr.hasNext) {
      val next: Node[String] = expr.next()

      if (exprType == null) exprType = next.kind
      else {
        if (op == null) op = next.value
        else {
          op = next.kind
          if (op == null) op = next.value
        }
      }

      exprType = this.evaluate(exprType, op)
    }
    exprType
  }

  def handleExpr(root: Node[String], scope: Symbol): Node[String] = {
    root.value match {
      case "ID" =>
        val id: String = this.readTerms("ID").pop()
        val typeInScope: String = scope.getType(id)
        root.kind = typeInScope
        return root
      case "NUM" =>
        root.kind = "INT"
        return root
      case "NULL" =>
        root.kind = "INT STAR"
        return root
      case _ =>
    }

    var functionScope: Procedure = null

    if (root.children.exists(child => child.value == "ID") &&
      root.children.exists(child => child.value == "arglist")) {
      val functionName: String = this.readTerms("ID").top()

      functionScope = this.symbolTable.scope.find(sym => sym.name == functionName)
        .head.asInstanceOf[Procedure]
    }

    root.children.map(child => {
      if (child.value == "arglist") {
        /*root.kind = this.handleArgs(child, functionScope).kind
        return root*/
        this.handleArgs(child, functionScope)
      }
      else this.handleExpr(child, scope)
    })
    if (root.children.nonEmpty) root.kind = this.resolveExpr(root.children.toIterator)
    if (!this.types.contains(root.kind))
      throw new Exception("Incomplete or wrong expression!")
    root
  }

  def handlePrint(root: Node[String], scope: Symbol): Node[String] = {
    val printType: String = this.handleExpr(root.children.find(child => child.value == "expr").head, scope).kind
    if (printType != "INT") throw new Exception("Cannot print " + printType + ".")
    root
  }

  def handleDelete(root: Node[String], scope: Symbol): Node[String] = {
    val printType: String = this.handleExpr(root.children.find(child => child.value == "expr").head, scope).kind
    if (printType != "INT STAR") throw new Exception("Cannot delete " + printType + ".")
    root
  }

  def handleStatements(root: Node[String], scope: Symbol): Node[String] = {
    if (root.children.isEmpty) return root
    root.children.head.value match {
      case "PRINTLN" => return this.handlePrint(root, scope)
      case "DELETE" => return this.handleDelete(root, scope)
      case _ =>
    }

    if (root.children.head.value == "lvalue") {
      val lvalueType: String = this.handleExpr(root.children.head, scope).kind
      val exprType: String = this.handleExpr(root.children(2), scope).kind
      if (lvalueType != exprType) throw new Exception("Cannot assign lvalue of type "
      + lvalueType + " to expression of type " + exprType + ".")
      return root
    }

    root.value match {
      case "statement" | "statements" | "IF" | "WHILE" =>
        root.children.map(child => this.handleStatements(child, scope))
      case "expr" | "lvalue" => root.kind = this.handleExpr(root, scope).kind
      case "test" => this.handleTest(root, scope)
    }

    root
  }

  def handleTest(root: Node[String], scope: Symbol): Unit = {
    val rhs: String = this.handleExpr(root.children.head, scope).kind
    val lhs: String = this.handleExpr(root.children(2), scope).kind
    val comp: String = root.children(1).value
    if (lhs != rhs)
      throw new Exception("Cannot compare types of " + lhs + " and " + rhs + ".")
    comp match {
      case "LT" | "GT" | "LE" | "GE" | "EQ" | "NE" =>
      case op => throw new Exception("Comparison operator " + op + " not defined.")
    }
  }

  def handleDcls(root: Node[String], scope: Symbol): Unit = {
    if (root.children.exists(child => child.value == "BECOMES")) {
      val rhs: String = root.children(3).value match {
        case "NUM" => "INT"
        case "NULL" => "INT STAR"
      }
      val lhs: String = this.typeResolve(root.children(1)
        .children.head.children.map(child => child.value))
      if (lhs != rhs) throw new Exception("Cannot initialize " + lhs + " to " + rhs + ".")
    }
    else root.children.foreach(child => this.handleDcls(child, scope))
  }

  def augmentTree(root: Node[String], inScope: Symbol): Node[String] = {
    var scope: Symbol = inScope
    root.value match {
      case "procedure" =>
        scope = inScope.scope
          .find(sym => sym.name == this.readTerms("ID").pop()).toSeq.head
      case "main" =>
        scope = inScope.scope
          .find(sym => sym.name == "wain").toSeq.head
        if (scope.asInstanceOf[Procedure].params.scope(1).kind != "INT")
          throw new Exception("Second parameter to wain should be INT.")
      case "expr" =>
        return this.handleExpr(root, scope)
      case "statements" => return this.handleStatements(root, scope)
      case "dcls" =>
        this.handleDcls(root, scope)
        root.children.foreach(child => {
          if (child.value == "ID") this.readTerms("ID").pop()})
      case _ => root.children.foreach(child => {
        if (child.value == "ID") this.readTerms("ID").pop()
      })
    }

    root.children.map(child => this.augmentTree(child, scope))

    if (root.children.exists(elem => elem.value == "RETURN")) {
      val returnInx: Int = root.children.indexOf(root.children.find(elem => elem.value == "RETURN").toSeq.head)
      val actualReturn: String = root.children(returnInx + 1).kind
      if (scope.kind != actualReturn) throw new Exception("Function " + scope.name + " has to return type " +
        scope.kind + " instead of " + actualReturn + ".")
    }

    root
  }

  def wlp4parse(input: Iterator[String] = null): Unit = {
    this.carry = Seq.empty
    this.unread = Iterator("BOF BOF")
    if (input != null) this.unread = this.unread.++(input)
    else this.unread = this.unread.++(io.Source.stdin.getLines())
    this.unread = this.unread.++(Iterator("EOF EOF"))

    try {
      this.fillStates(this.stateCount)
    } catch {
      case e: Exception =>
        println(e.getMessage)
        System.exit(1)
    }
    try {
      while (this.unread.hasNext | this.inputCarry) {
        val in: String = read()
        this.inputCarry = false
        if (in != "") {
          this.position = 0
          val raw: Seq[String] = in.split(" +").filter(sym => sym != "").toSeq
          this.words = raw.toIterator
          while (this.parse()) {
            this.position += 1
          }
        }
      }
    } catch {
      case _: NoSuchElementException =>
      case e: Exception => System.err.println(e.getMessage)
    }
  }

  def reArrangeTree(curr: Node[String]): Node[String] = {
    val nonTermIndexes: Seq[Int] = curr.children
      .filter(item => nonTerminals.contains(item.value)).map(item => curr.children.indexOf(item))
    val nonTermIndexesRev: Seq[Int] = nonTermIndexes.reverse

    var newRHS: Seq[Node[String]] = curr.children
    for (i <- nonTermIndexes.indices) {
      newRHS = newRHS.updated(nonTermIndexes(i), curr.children(nonTermIndexesRev(i)))
    }
    curr.children = newRHS
    curr
  }

  def wrapArrange(root: Node[String]): Node[String] = {
    root.children.map(wrapArrange)
    val newRoot: Node[String] = this.reArrangeTree(root)
    newRoot
  }

  // TODO : Temp value to pass read terms to code gen
  var genStack: Map[String, Stack[String]] = Map.empty

  def gen(): Node[String] = {
    this.reArrangeOutput()
    var root: Node[String] = this.genTree("start")

    this.readTerms.foreach(item => {item._2.reverse()})

    this.readTerms.foreach(item => {
      val newStack: Stack[String] = new Stack[String]
      item._2.stack.reverse.foreach(item => newStack.push(item))
      this.genStack += item._1 -> newStack
    })

    this.buildSymbolTable(root, this.symbolTable)
    this.readTerms = this.readTermsTemp
    this.readTerms.foreach(item => item._2.reverse())
    root = this.augmentTree(root, this.symbolTable)
    root
  }

  def wlp4gen(input: Iterator[String]): Node[String] = {
    this.unread = input
    try {
      while (this.unread.hasNext) {
        val in: String = this.fullRead()
        if (this.nonTerminals.contains(in.split(" +").toSeq.head)) {
          val transition: Transition = new Transition(in)
          this.output.add(transition)
        }
      }
      this.gen()
    } catch {
      case _: NoSuchElementException =>
        null
      case e: Exception =>
        System.err.println("[ERROR] " + e.getMessage)
        null
    }
  }

  def main(args: Array[String]): Unit = {
    /* TODO : If wlp4 code is given
    this.carry = WLP4Scan.run().reverse
    wlp4parse(this.carry.toIterator)
    this.parsed = this.parsed.:+("EOF EOF")
    this.parsed = this.parsed.tail.+:("BOF BOF")
    this.parsed = this.parsed.+:("start BOF procedures EOF")*/
    var parseTree: Node[String] = wlp4gen(io.Source.stdin.getLines())
    //try {
      Gen.readStack = this.genStack
      parseTree = Gen.run(parseTree, this.symbolTable)
    /*} catch {
      case e: Exception =>
        System.err.println("[ERROR] " + e.getMessage)
    }*/
  }

    //##################################################################################################################
    //##################################################################################################################
    //##################################################################################################################
    //##################################################################################################################
    //##################################################################################################################
    //##################################################################################################################

    object MIPS {

      // TODO : Store 4 in $4
      this.lis(4)
      this.word(4)

      // TODO : Store 1 in $11
      //this.lis(11)
      //this.word(1)

      def sign(unsigned: Boolean): String = {
        if (unsigned) "u"
        else ""
      }
      def comment(string: String): Unit = {
        System.out.println("\n; " + string)
      }
      def inject(string: String): Unit = {
        System.out.println(string)
      }
      def rFormat(name: String, source: Int, op1: Int, op2: Int): Unit = {
        System.out.println(name + " $" + source + ", $" + op1 + ", $" + op2)
      }
      def twoOp(name: String, op1: Int, op2: Int): Unit = {
        System.out.println(name + " $" + op1 + ", $" + op2)
      }
      def oneOp(name: String, op: Int): Unit = {
        System.out.println(name + " $" + op)
      }
      def iFormat(name: String, op1: Int, op2: Int, i: Int, offset: Boolean): Unit = {
        if (offset) System.out.println(name + " $" + op1 + ", " + i + "($" + op2 + ")")
        else System.out.println(name + " $" + op1 + ", $" + op2 + ", " + i)
      }
      def iFormat(name: String, op1: Int, op2: Int, i: String, offset: Boolean): Unit = {
        if (offset) System.out.println(name + " $" + op1 + ", " + i + "($" + op2 + ")")
        else System.out.println(name + " $" + op1 + ", $" + op2 + ", " + i)
      }
      def word(i: Int): Unit = {
        System.out.println(".word " + "0x" + i.toHexString)
      }
      def word(string: String): Unit = {
        System.out.println(".word " + string)
      }
      def add(source: Int, op1: Int, op2: Int): Unit = {
        this.rFormat("add", source, op1, op2)
      }
      def sub(source: Int, op1: Int, op2: Int): Unit = {
        this.rFormat("sub", source, op1, op2)
      }
      def mult(op1: Int, op2: Int, unsigned: Boolean = false): Unit = {
        this.twoOp("mult" + this.sign(unsigned), op1, op2)
      }
      def div(op1: Int, op2: Int, unsigned: Boolean = false): Unit = {
        this.twoOp("div" + this.sign(unsigned), op1, op2)
      }
      def mfhi(op: Int): Unit = {
        this.oneOp("mfhi", op)
      }
      def mflo(op: Int): Unit = {
        this.oneOp("mflo", op)
      }
      def lis(op: Int): Unit = {
        this.oneOp("lis", op)
      }
      def lw(op1: Int, op2: Int, i: Int): Unit = {
        this.iFormat("lw", op1, op2, i, offset = true)
      }
      def sw(op1: Int, op2: Int, i: Int): Unit = {
        this.iFormat("sw", op1, op2, i, offset = true)
      }
      def slt(source: Int, op1: Int, op2: Int, unsigned: Boolean = false): Unit = {
        this.rFormat("slt" + this.sign(unsigned), source, op1, op2)
      }
      def beq(op1: Int, op2: Int, i: Int): Unit = {
        this.iFormat("beq", op1, op2, i, offset = false)
      }
      def beq(op1: Int, op2: Int, i: String): Unit = {
        this.iFormat("beq", op1, op2, i, offset = false)
      }
      def bne(op1: Int, op2: Int, i: Int): Unit = {
        this.iFormat("bne", op1, op2, i, offset = false)
      }
      def jr(op: Int): Unit = {
        this.oneOp("jr", op)
      }
      def jalr(op: Int): Unit = {
        this.oneOp("jalr", op)
      }
    }
    //##################################################################################################################
    //##################################################################################################################
    //##################################################################################################################
    //##################################################################################################################
    //##################################################################################################################
    // ##################################################################################################################

  object Gen {

    var readStack: Map[String, Stack[String]] = _


    def run(root: Node[String], scope: Symbol): Node[String] = {
      root.value match {
        case "main" =>
          val frame: Frame = new Frame(scope.get("wain").asInstanceOf[Procedure])
          return frame.body(root)
        case _ =>
      }
      root.children.foreach(child => run(child, scope))
      root
    }

    val PLUS = 10
    val MINUS = 11
    val STAR = 12
    val SLASH = 13
    val PCT = 14
    val PRINTLN = 15
    val BECOME = 16
    val WHILE = 17
    val AMP = 18
    val NEW = 19
    val DELETE = 20

    var lastOffset: Int = 0
    var loopCount: Int = 0
    var labelCount: Int = 0

    var init: Boolean = false

    class Frame(var scope: Procedure) {

      val wain: Boolean = this.scope.name == "wain"
      this.scope = scope
      val fp: Int = 29
      val fpLen: Int = 28
      val sp: Int = 30
      val orgRtn: Int = 31
      var assignLength: Boolean = false

      var routineStack: Stack[Routine] = new Stack[Routine]

      class Routine(eval: Node[String], back: String, runnable: Int) {

        val name: String = "routine" + loopCount
        private var terminator: String = "end" + name
        loopCount += 1

        var terminatorDeclared: Boolean = false

        def save(): Unit = {
          routineStack.push(this)
        }

        def setTerminator(terminator: String): Unit = {
          this.terminator = terminator
          this.terminatorDeclared = true
        }

        def getTerminator: String = this.terminator

        def run(): Unit = {}

        def loop(): String = {
          MIPS.inject("\n")
          MIPS.inject(name + ":")
          genStatements(eval.children(runnable))
          if (back != null) MIPS.beq(0, 0, back)
          if (!this.terminatorDeclared) MIPS.inject(terminator + ":")
          terminator
        }
      }

      class Test(eval: Node[String]) {
        val name: String = "test" + labelCount
        labelCount += 1

        var pointer: Boolean = eval.children.exists(child => child.kind == "INT STAR")

        def run(): Unit = {
          MIPS.comment("Making test")
          MIPS.inject(name + ":")
          code(eval.children.head)
          code(eval.children(2))
          eval.children(1).value match {
            case "LT" => LT()
            case "GT" => GT()
            case "EQ" => EQ()
            case "NE" => NE()
            case "LE" => MIPS.add(Intm.intm1, EQ(6), LT(7, Intm.intm2))
            case "GE" => MIPS.add(Intm.intm1, EQ(6), GT(7, Intm.intm2))
          }
        }

        def LT(dest: Int = Intm.intm1, op: Int = Intm.five): Int = {
          MIPS.slt(dest, op, Intm.intm1, unsigned = pointer)
          dest
        }
        def GT(dest: Int = Intm.intm1, op: Int = Intm.five): Int = {
          MIPS.slt(dest, Intm.intm1, op, unsigned = pointer)
          dest
        }
        def EQ(dest: Int = Intm.intm1): Int = {
          val eqResult: Int = NE(dest)
          MIPS.sub(dest, 11, eqResult)
          eqResult
        }
        def NE(dest: Int = Intm.intm1): Int = {
          LT(6)
          GT(7, Intm.intm2)
          MIPS.add(dest, 6, 7)
          dest
        }
      }

      object Intm {
        val intm1: Int = 3
        val intm2: Int = 5

        var oneEmpty: Boolean = true
        var twoEmpty: Boolean = true

        var addrHistory: Int = 0

        def three: Int = {
          intm1
        }

        def five: Int = {
          MIPS.add(sp, sp, 4)
          MIPS.lw(intm2, sp, addrHistory)
          intm2
        }

        def getForStore: Int = {
          if (!oneEmpty) {
            MIPS.sw(intm1, sp, -4)
            MIPS.sub(sp, sp, 4)
            this.addrHistory = -4
          }
          oneEmpty = false
          intm1
        }
      }

      var addrOffset: Int = 0
      var lastAvailAddr: Int = 0

      def prolog(): Unit = {
        // Importing print
        MIPS.inject(".import print")
        MIPS.inject(".import init")
        MIPS.inject(".import new")
        MIPS.inject(".import delete")
        MIPS.lis(11)
        MIPS.word(1)

        if (!init) {
          if (this.scope.name == "wain") {
            if (this.scope.params.scope.head.kind == "INT") {
              MIPS.add(2, 0, 0)
            } else {
              // TODO : Make sure $2 is not changed
            }
          }
          this.jump("init")
        }

        MIPS.sub(fp, sp, 4)
        val elemCount: Int = (this.scope.scope.length +
          this.scope.params.scope.length) * 4 * 2
        MIPS.comment("Making room in stack frame.")
        MIPS.sub(sp, sp, this.num(elemCount, forFp = true))

        // TODO : Set $28 to other half
        MIPS.sub(fpLen, 29, this.num(elemCount, forFp = true))

        if (this.wain) {
          MIPS.comment("Storing wain params to frame.")
          MIPS.sw(1, fp, this.scope.params.scope.head.address - addrOffset)
          MIPS.sw(2, fp, this.scope.params.scope(1).address - addrOffset)
        } else {
          this.push(-1)
          addrOffset = -4
        }
      }

      def epilog(): Unit = {
        MIPS.lw(1, fp, this.scope.params.scope.head.address - addrOffset)

        MIPS.add(sp, fp, 4)
        if (!this.wain) MIPS.lw(orgRtn, fp, 0)

        for (i <- 6 to 10) {
          MIPS.add(i, 0, 0)
        }

        for (i <- 12 to 28) {
          MIPS.add(i, 0, 0)
        }

        MIPS.jr(31)

        while (this.routineStack.stack.nonEmpty) {
          this.routineStack.pop().run()
        }
      }

      def body(function: Node[String]): Node[String] = {
        var carryAddr: Int = 0
        this.scope.params.scope.map(sym => {
          sym.address = carryAddr
          carryAddr -= 4
          sym
        })

        this.prolog()

        // TODO : Getting the params out
        readStack("ID").pop()
        readStack("ID").pop()

        this.scope.scope.map(sym => {
          sym.address = carryAddr
          carryAddr -= 4
          sym
        })

        function.children.filter(child => nonTerminals.contains(child.value)).foreach(child => this.code(child))

        this.epilog()

        function
      }

      def genMaterial(root: Node[String]): Seq[Int] = {
        var resolveMaterial: Seq[Int] = Seq.empty

        var convertInt: Boolean = false

        if (root.children.exists(child => (child.value == "PLUS") || (child.value == "MINUS")) &&
          root.children.exists(child => child.kind == "INT STAR")) {
          convertInt = true
        }

        root.children.foreach(child => {
          val result: Int = this.code(child)
          resolveMaterial = resolveMaterial.+:(result)
          if (child.kind == "INT" && convertInt) {
            MIPS.mult(result, 4)
            MIPS.mflo(result)
          }
        })
        resolveMaterial
      }

      def code(root: Node[String]): Int = {
        root.value match {
          case "ID" =>
            val id: String = readStack("ID").pop()
            lastOffset = this.scope.get(id).address - addrOffset
            this.get(id, Intm.three)
          case "NUM" =>
            this.push(readStack("NUM").pop().toInt)
          case "NULL" => 0x01
          case "PLUS" => PLUS
          case "MINUS" => MINUS
          case "STAR" => STAR // TODO : Consider for de-referencing
          case "AMP" => AMP
          case "SLASH" => SLASH
          case "PCT" => PCT
          case "NEW" => NEW
          case "expr" | "term" | "factor" | "lvalue" =>
            this.resolve(this.genMaterial(root))
          case "statements" => this.genStatements(root)
          case "dcls" =>
            this.genDcls(root)
          case "dcl" => 16
          case _ =>
            -1
        }
      }




      def genDcls(root: Node[String]): Int = {
        root.value match {
          case "dcls" =>
            if (root.children.nonEmpty) this.genDcls(root.children.head)
            else return -1
            var resolveMaterial: Seq[Int] = Seq.empty
            root.children.tail.foreach(child => {
              resolveMaterial = resolveMaterial.:+(this.genDcls(child))
            })
            this.resolveDcls(resolveMaterial)
          case "dcl" => 16
          case "NUM" =>
            this.num(readStack("NUM").pop().toInt)
          case "NULL" => 0x01
          case "expr" => this.code(root)
          case _ => -1
        }
      }

      def resolveDcls(vals: Seq[Int]): Int = {
        this.dcl(readStack("ID").pop(), Intm.intm1)
        -1
      }

      def resolveAssignment(vals: Seq[Int]): Int = {
        MIPS.comment("Resolving assignment")
        vals.head match {
          case 1 =>
            MIPS.sw(Intm.intm1, Intm.five, 0)
            return -1
          case _ =>
        }
        if (assignLength) {
          MIPS.sw(13, fpLen, vals.head)
          this.assignLength = false
        }
        MIPS.sw(Intm.intm1, fp, vals.head)
        -1
      }

      def resolveLvalue(vals: Seq[Int]): Int = {
        if (vals.length == 2) {
          val result: Int = vals.head match {
            case STAR =>
              MIPS.comment("Assigning a value to a pointer's address.")
              1 // TODO : 0 means retrieve the value on $3 and use as pointer address
            case _ => this.refResolve(vals(1), vals.head)
          }
          if (result == -1)
            vals.find(value => value < 1).toSeq.head
          else result
        }
        else vals.find(value => value < 1 ).toSeq.head
      }

      def simplifyLvalue(root: Node[String]): Int = {
        root.value match {
          case "ID" =>
            this.scope.get(readStack("ID").pop()).address - addrOffset
          case "lvalue" =>
            var resolveMaterial: Seq[Int] = Seq.empty
            root.children.foreach(child => {
              val simplified: Int = this.simplifyLvalue(child)
              resolveMaterial = resolveMaterial.:+(simplified)
            })
            val lval: Int = this.resolveLvalue(resolveMaterial)
            lval
          case "STAR" => STAR
          case "factor" => this.code(root)
          case _ => 1
        }
      }

      def genStatements(root: Node[String]): Int = {

        if (root.children.nonEmpty) {
          root.children.head.value match {
            case "WHILE" => return this.resolveCondition(root)
            case "IF" => return this.resolveCondition(root, isIf = true)
            case _ =>
          }
        }

        root.value match {
          case "expr" => this.code(root)
          case "lvalue" => this.simplifyLvalue(root)
          case "PRINTLN" => PRINTLN
          case "BECOMES" => BECOME
          case "dcls" => genDcls(root)
          case "WHILE" => WHILE
          case "DELETE" => DELETE
          case "statement" =>
            var resolveMaterial: Seq[Int] = Seq.empty
            root.children.foreach(child => {
              resolveMaterial = resolveMaterial.:+(this.genStatements(child))
            })
            this.resolveStatement(resolveMaterial)
            1
          case _ =>
            root.children.foreach(this.genStatements)
            -1
        }
      }

      def resolveStatement(vals: Seq[Int]): Int = {
        vals.head match {
          case PRINTLN =>
            this.resolvePrint(vals.tail)
          case DELETE =>
            // TODO : $1 is base of array
            // TODO : Check $1 for NULL
            // TODO : Must delete the whole array

            MIPS.add(14, 1, 0)
            MIPS.add(1, 3, 0)

            MIPS.lw(13, fpLen, lastOffset)
            MIPS.mult(13, 4)
            MIPS.mflo(13) // TODO : $13 is the length

            val whileTestLabel: String = new Test(null).name
            val whileEndLabel: String = new Routine(null, whileTestLabel, 0).getTerminator
            MIPS.inject(whileTestLabel + ":")
            MIPS.slt(Intm.intm1, 1, 13)
            MIPS.beq(3, 0, whileEndLabel)

            this.jump("delete")

            MIPS.add(1, 1, 4)
            MIPS.beq(0, 0, whileTestLabel)
            MIPS.inject(whileEndLabel + ":")

            // TODO : Setting array to NULL
            MIPS.lis(Intm.intm1)
            MIPS.word(0x01)
            MIPS.sw(Intm.intm1, fp, lastOffset)
            MIPS.add(1, 14, 0)

            -1
          case _ =>
            if (vals.contains(BECOME)) this.resolveAssignment(vals)
            else -1
        }
      }


      def countLines(seq: Iterator[Node[String]]): Int = {
        var count: Int = 0
        while (seq.hasNext) {
          val curr: Node[String] = seq.next()
          if (curr.value == "statement") count +=1
          count += this.countLines(curr.children.toIterator)
        }
        count
      }

      def resolveCondition(root: Node[String], isIf: Boolean = false): Int = {

        if (isIf) {
          val condition: Test = new Test(root.children(2))
          val routine1: Routine = new Routine(root, null, runnable = 5)
          val routine2: Routine = new Routine(root, null, runnable = 9)
          routine1.setTerminator(routine2.name)

          condition.run()
          MIPS.beq(Intm.intm1, 0, routine1.getTerminator)
          routine1.loop()
          MIPS.beq(0, 0, routine2.getTerminator)
          routine2.loop()

        } else {
          val condition: Test = new Test(root.children(2))
          val loopRoutine: Routine = new Routine(root, condition.name, runnable = 5)

          condition.run()
          MIPS.beq(Intm.intm1, 0, loopRoutine.getTerminator)
          loopRoutine.loop()
        }

        -1
      }

      def resolvePrint(vals: Seq[Int]): Int = {
        val regWithData: Int = vals.find(value => value > 1).toSeq.head
        MIPS.add(14, 1, 0)
        MIPS.add(1, regWithData, 0)

        this.jump("print")

        MIPS.add(1, 14, 0)
        -1
      }

      def opResolve(lhs: Int, op: Int, rhs: Int): Int = {
        MIPS.comment("Asking for a register to store op result in")
        val regUsed: Int = Intm.three
        op match {
          case PLUS =>
            MIPS.add(regUsed, rhs, lhs)
          case MINUS =>
            MIPS.sub(regUsed, rhs, lhs)
          case STAR =>
            MIPS.mult(rhs, lhs)
            MIPS.mflo(regUsed)
          case SLASH =>
            MIPS.div(rhs, lhs)
            MIPS.mflo(regUsed)
          case PCT =>
            MIPS.div(rhs, lhs)
            MIPS.mfhi(regUsed)
          case Intm.intm1 => Intm.intm1
          case BECOME =>
            MIPS.sw(rhs, fp, lhs)
        }
        regUsed
      }

      def refResolve(rhs: Int, op: Int): Int = {
        MIPS.comment("Asking for a register to store op result in")
        val regUsed: Int = rhs
        if (regUsed != Intm.intm1) throw new Exception("Unrecognized register " + rhs + ".")
        op match {
          case AMP =>
            if (lastOffset != -1) this.push(lastOffset, pointer = true)
            else MIPS.add(3, 14, 0)
          case STAR =>
            lastOffset = -1
            MIPS.add(14, regUsed, 0)
            MIPS.lw(regUsed, regUsed, 0)
          case 1 => return -1
        }
        regUsed
      }

      def allocMem(): Int = {
        // TODO : $3 is size of array

        MIPS.add(14, 1, 0)
        MIPS.add(1, 3, 0)

        this.jump("new")

        MIPS.add(13, 1, 0)
        this.assignLength = true
        // TODO : Later in assignment add $13 as the new array length to fpLen
        MIPS.add(1, 14, 0)

        // TODO : In $3: 0 if exhausted, otherwise head of array

        Intm.intm1
      }

      def resolve(vals: Seq[Int]): Int = {
        if (vals.length == 1) vals.head
        else if (vals.reverse.head == NEW) this.allocMem()
        else if (vals.length == 2 && (vals.contains(AMP) || vals.contains(STAR)))
          this.refResolve(vals.head, vals(1))
        else if (vals.length == 2) throw new Exception("Binary operator needs two" +
          "operands")
        else {
          if (vals.forall(i => i > 2))
            this.opResolve(Intm.three, vals(1), Intm.five)
          else vals(1)
        }
      }

      def push(value: Int, id: String = null, pointer: Boolean = false): Int = {
        if (id == null && value == -1) {
          MIPS.sw(orgRtn, fp, 0)
          -1
        }
        else {
          var address: Int = -1
          val reg: Int = this.num(value)
          if (id == null) {
            if (pointer) MIPS.add(Intm.intm1, reg, fp)
            else this.lastAvailAddr += 4
          }
          else {
            address = this.scope.get(id).address - addrOffset
            MIPS.sw(reg, fp, address)
          }
          reg
        }
      }

      def get(id: String, reg: Int): Int = {
        if (reg == Intm.intm1) Intm.getForStore
        MIPS.comment("Getting var " + id + " from stack into reg $" + reg + ".")
        MIPS.lw(reg, this.fp, this.scope.get(id).address - addrOffset)
        reg
      }

      def dcl(id: String, reg: Int): Int = {
        MIPS.comment("Declaring variable " + id + ".")
        MIPS.sw(reg, this.fp, this.scope.get(id).address - addrOffset)
        reg
      }

      def num(value: Int, forFp: Boolean = false): Int = {
        MIPS.comment("Putting value " + value + " in a reg.")
        var destReg: Int = -1
        if (forFp) destReg = 12
        else destReg = Intm.getForStore
        MIPS.lis(destReg)
        MIPS.word(value)
        destReg
      }

      def jump(name: String): Unit = {
        MIPS.sw(31, sp, -4)
        MIPS.sub(sp, sp, 4)

        MIPS.lis(16)
        MIPS.word(name)
        MIPS.jalr(16)

        MIPS.lw(31, sp, 0)
        MIPS.add(sp, sp, 4)
      }
    }
  }

}
