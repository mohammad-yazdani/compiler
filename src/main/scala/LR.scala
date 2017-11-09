object LR {

  class Transition (rawProduction: String) {
    val LHS: String = null
    val RHS: Seq[String] = Seq.empty
    // TODO : Make LHS: String, RHS: Seq[String]

  }

  class Stack {
    private var stack: Seq[String] = Seq.empty

    def push(item: String): Unit = {
      this.stack.+:(item)
    }

    def pop(): String = {
      val temp = stack.head
      stack = stack.tail
      temp
    }
  }

  val cfgrl = new CFGRL()

  val terminals: Set[String] = cfgrl.terminals
  val nonTerminals: Set[String] = cfgrl.nonTerminals
  val start: String = cfgrl.start
  val transitions: Seq[Transition] = Seq.empty
  cfgrl.transitionsRaw.map(production => transitions.:+(new Transition(production)))

  val unread: Iterator[String] = cfgrl.input
  var ahead: String = _
  var symbolStack: Stack = new Stack()
  var stateStack: Stack = new Stack()

  val predictTable: Map[Map[String, String], String] = Map.empty

  def read(): String = {
    if (this.ahead != null) {
      val temp: String = this.ahead
      this.ahead = _
      return temp
    }
    this.unread.next()
  }

  def lookAhead(): String = {
    if (this.ahead == null)
      this.ahead = this.unread.next()
    this.ahead
  }

  def doMatch(): Boolean = {
    _
  }

  def empty(nonTerm: String): Boolean = {
    _
  }

  def first(nonTerm: String): String = {
    this.transitions
      .filter(transition => transition.LHS == nonTerm)
      .find(transition =>
        transition.RHS.exists(elem => terminals.contains(elem))).head
      .RHS.find(elem => terminals.contains(elem)).head
  }

  def follow(nonTerm: String): Boolean = {
    // TODO : Relevant if empty(in) is true
    _
  }

  def main(args: Array[String]): Unit = {
    this.symbolStack.push("BOF")
    while (true) {
      this.symbolStack.push(this.unread.next())
    }
  }
}
