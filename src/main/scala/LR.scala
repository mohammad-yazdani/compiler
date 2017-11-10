import scala.annotation.tailrec

object LR {

  /***
    * CFGRL starter code for CS241 A7
    * Based on cfgrl.rkt by Gordon V. Cormack
    * Created by Sean Harrap for CS241 in Winter 2017.
    */
  class CFGRL {
    private case class ParseTree(tok: String, children: Seq[ParseTree]) {
      //Performs a preorder traversal of a parse tree, where the integer
      //associated with each tree is its level of indentation.
      //The list "s" is a stack.
      def preorder(): Unit = {
        var s: List[(ParseTree,Int)] = List((this,0))
        while (s.nonEmpty) {
          val (p,depth) = s.head
          s = p.children.toList.map((_,1+depth)) ++ s.tail
          println(" " * depth + p.tok)
        }
      }
    }

    var terminals: Set[String] = Set.empty
    var nonTerminals: Set[String] = Set.empty
    var transitionsRaw: Seq[String] = Seq.empty
    var start: String = _

    val input: Iterator[String] = io.Source.stdin.getLines()
    private val numTerminals: Int = input.next().toInt
    (1 to numTerminals).foreach(_ => this.terminals += input.next())   // Print terminals
    private val numNonterminals: Int = input.next().toInt
    (1 to numNonterminals).foreach(_ => this.nonTerminals += input.next())
    private val startSymbol: String = input.next()
    this.start = startSymbol
    private val numProductions: Int = input.next().toInt
    (1 to numProductions).foreach(_ => this.transitionsRaw = this.transitionsRaw.:+(input.next())) // Print productions

    //Reads a tree recursively from stdin
    @tailrec private def readTree(stack: List[ParseTree]): ParseTree = {
      val ln = input.next()
      val lnProd = ln.split(' ')
      val times = lnProd.tail.count(this.nonTerminals.contains) //Number of children at this node
      if (lnProd.head == startSymbol) ParseTree(ln,Seq(stack.head))
      else readTree(ParseTree(ln,stack.take(times).reverse) :: stack.drop(times))
    }

    //Read the tree and print its preorder traversal
    def printTree(): Unit = {
      while (input.hasNext) readTree(Nil).preorder()
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

  val cfgrl = new CFGRL()

  val terminals: Set[String] = cfgrl.terminals
  val nonTerminals: Set[String] = cfgrl.nonTerminals
  val start: String = cfgrl.start
  var transitions: Seq[Transition] = Seq.empty
  cfgrl.transitionsRaw.foreach(production => this.transitions = this.transitions.:+(new Transition(production)))

  val unread: Iterator[String] = cfgrl.input
  var words: Iterator[String] = Iterator.empty
  var ahead: String = _
  var symbolStack: Stack[String] = new Stack[String]
  var states: Array[State] = Array.fill[State](read().toInt)(null)
  val stateCount: Int = read().toInt

  var stateStack: Stack[Int] = new Stack[Int]

  var output: Stack[String] = new Stack[String]
  var position: Int = _
  var haveRead: Int = 0

  var inputCarry: Boolean = false

  def fillStates(count: Int): Unit = {
    for (_ <- 0 until count) {
      val raw: Seq[String] = read().split(" +").toSeq

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
          System.exit(1)
      }
    }
  }

  def read(): String = {
    this.unread.next()
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

  def recur(lhs: String, sym: String, term: String, carry: String, visited: Set[String]): Int = {
    var result: Int = 0
    var newVisited: Set[String] = visited
    if (newVisited.contains(lhs)) return 0
    else newVisited += lhs
    var rules: Set[Transition] = Set.empty
    this.transitions.foreach(trans => {
      if (trans.LHS == lhs) {
        rules += trans
      }
    })
    for (trans <- rules) {
      var newCarry: String = carry
      var restNonTerms: Set[String] = Set.empty
      trans.RHS.foreach(elem => {
        if (nonTerminals.contains(elem)) restNonTerms += elem
      })

      if (carry != null) {
        if (trans.RHS.head == carry)
          result += 1
        else if (this.nonTerminals.contains(trans.RHS.head)) {
          result += recur(trans.RHS.head, sym, term, carry = carry, visited = newVisited)
          restNonTerms -= trans.RHS.head
        }
        else {
          newCarry = null
        }
      }

      if (trans.LHS == sym && trans.RHS.head == term)
        result += 1

      if (trans.RHS.contains(sym) && trans.RHS.contains(term)) {
        if (trans.RHS.indexOf(sym) == (trans.RHS.indexOf(term) - 1))
          result += 1
      }

      if (trans.RHS.contains(term) && trans.RHS.head != term) {
        val beforeTerm: String = trans.RHS(trans.RHS.indexOf(term) - 1)
        if (this.nonTerminals.contains(beforeTerm)) {
          result += recur(beforeTerm, sym, term, carry = sym, visited = newVisited)
          restNonTerms -= beforeTerm
        }
      }

      restNonTerms.foreach(tok => result += recur(tok, sym, term, newCarry, visited = newVisited))
    }
    result
  }

  def follow(sym: String, term: String): Boolean = {
    val search: Int = recur(this.start, sym, term, null, Set.empty)
    search > 0
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
      this.output.push(this.transitions(in.toInt).view)
      for (_ <- popped.indices) this.stateStack.pop()
      this.states(stateStack.top()).move(lhs)

      /*val next: String = this.lookAhead()
      val reducible: Boolean = this.follow(lhs, next)
      if (reducible) {


      }
      else {
        symbolStack.push(poppedRaw)
        this.shift(sym, stateStack.top())
      }*/
    } else throw new Exception("Wrong reduce.")
  }

  def parse(): Boolean = {
    if (this.symbolStack.stack.equals(this.transitions.head.RHS.reverse)) {
      this.output.push(this.transitions.head.view)
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
      case e: Exception =>
        println(e.getMessage)
        error()
        false
    }
  }

  def error(): Unit = {
    System.err.println("ERROR at " + this.haveRead)
  }

  def finish(): Unit = {
    this.output.stack.reverse.foreach(println)
    this.output.clear()
  }

  def main(args: Array[String]): Unit = {
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
      case e: Exception =>
        println(e.getMessage)
    }
  }
}
