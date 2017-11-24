import WLP4Gen.{Node, Procedure, Stack, nonTerminals}
import MIPS._

object Gen {

  var readStack: Map[String, Stack[String]] = _

  class Frame(var scope: Procedure) {
    this.scope = scope
    val fp: Int = 29
    val sp: Int = 30
    val orgRtn: Int = 31
    val intm1: Int = 3
    val intm2: Int = 5
    val wain: Boolean = this.scope.name == "wain"

    def prolog(): Unit = {
      sub(fp, sp, 4)
      sub(sp, sp, num(this.scope.scope.length))
      this.push(null, -1)

      if (this.wain) {
        sw(1, fp, this.scope.params.scope.head.address + 4)
        sw(2, fp, this.scope.params.scope(1).address + 4)
      }
    }

    def epilog(): Unit = {
      add(sp, fp, 4)
      lw(orgRtn, fp, 0)
    }

    def body(function: Node[String]): Unit = {
      var carryAddr: Int = 0
      if (!this.wain) {
        this.scope.params.scope.foreach(sym => {
          sym.address = carryAddr
          carryAddr -= 4
        })
      }
      this.prolog()

      this.scope.scope.foreach(sym => {
        sym.address = carryAddr
        carryAddr -= 4
      })

      function.children.filter(child => nonTerminals.contains(child.value)).map(child => this.code(child))
    }

    def code(root: Node[String]): Node[String] = {
      root.value match {
        case "ID" =>
          this.get(readStack("ID").pop(), intm1)
        case _ =>
      }

      root.children.map(code)
      root
    }

    def push(id: String, value: Int): Unit = {
      if (id == null && value == -1) sw(orgRtn, fp, 0)
      sw(num(value), fp, this.scope.get(id).address + 4)
    }

    def get(id: String, reg: Int): Unit = {
      lw(reg, this.fp, this.scope.get(id).address)
    }
  }
}
