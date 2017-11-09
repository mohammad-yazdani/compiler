import scala.annotation.tailrec

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

  val terminals: Set[String] = Set.empty
  var nonTerminals: Set[String] = Set.empty
  val transitionsRaw: Seq[String] = Seq.empty
  var start: String = _

  val input: Iterator[String] = io.Source.stdin.getLines()
  private val numTerminals: Int = input.next().toInt
  println(numTerminals)
  (1 to numTerminals).foreach(_ => this.terminals.+(input.next()))   // Print terminals
  private val numNonterminals: Int = input.next().toInt
  println(numNonterminals)
  private val nonterminals: Set[String] = (1 to numNonterminals).map(_ => input.next()).toSet
  this.nonTerminals = nonterminals
  private val startSymbol: String = input.next()
  this.start = startSymbol
  private val numProductions: Int = input.next().toInt
  println(numProductions)
  (1 to numProductions).foreach(_ => this.transitionsRaw.:+(input.next())) // Print productions

  //Reads a tree recursively from stdin
  @tailrec private def readTree(stack: List[ParseTree]): ParseTree = {
    val ln = input.next()
    val lnProd = ln.split(' ')
    val times = lnProd.tail.count(nonterminals.contains) //Number of children at this node
    if (lnProd.head == startSymbol) ParseTree(ln,Seq(stack.head))
    else readTree(ParseTree(ln,stack.take(times).reverse) :: stack.drop(times))
  }

  //Read the tree and print its preorder traversal
  while (input.hasNext) readTree(Nil).preorder()
}
