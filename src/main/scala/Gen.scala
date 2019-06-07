import WLP4Gen.{Node, Procedure, Stack, nonTerminals, Symbol}

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

  class Frame(var scope: Procedure) {
    val wain: Boolean = this.scope.name == "wain"
    this.scope = scope
    val fp: Int = 29
    val sp: Int = 30
    val orgRtn: Int = 31
    val intm1: Int = 3
    val intm2: Int = 5
    var addrOffset: Int = 0

    def prolog(): Unit = {
      MIPS.sub(fp, sp, 4)
      val elemCount: Int = (this.scope.scope.length + this.scope.params.scope.length) * 4
      MIPS.sub(sp, sp, MIPS.num(elemCount))

      if (this.wain) {
        MIPS.sw(1, fp, this.scope.params.scope.head.address - addrOffset)
        MIPS.sw(2, fp, this.scope.params.scope(1).address - addrOffset)
      } else {
        this.push(null, -1)
        addrOffset = -4
      }
    }

    def epilog(): Unit = {
      MIPS.add(sp, fp, 4)
      if (!this.wain) MIPS.lw(orgRtn, fp, 0)
      MIPS.jr(31)
    }

    def body(function: Node[String]): Node[String] = {
      var carryAddr: Int = 0
      this.scope.params.scope.map(sym => {
        sym.address = carryAddr
        carryAddr -= 4
        sym
      })

      this.prolog()

      this.scope.scope.map(sym => {
        sym.address = carryAddr
        carryAddr -= 4
        sym
      })

      function.children.filter(child => nonTerminals.contains(child.value)).foreach(child => this.code(child))

      this.epilog()

      function
    }

    def code(root: Node[String]): Unit = {
      root.value match {
        case "ID" =>
          this.get(readStack("ID").pop(), intm1)
        case "expr" | "term" | "factor" =>
          root.children.foreach(code)
        case _ =>
      }
    }

    def push(id: String, value: Int): Unit = {
      if (id == null && value == -1) MIPS.sw(orgRtn, fp, 0)
      else MIPS.sw(MIPS.num(value), fp, this.scope.get(id).address - addrOffset)
    }

    def get(id: String, reg: Int): Unit = {
      MIPS.lw(reg, this.fp, this.scope.get(id).address - addrOffset)
    }
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
    def rFormat(name: String, source: Int, op1: Int, op2: Int): Unit = {
      println(name + " $" + source + ", $" + op1 + ", $" + op2)
    }
    def twoOp(name: String, op1: Int, op2: Int): Unit = {
      println(name + " $" + op1 + ", $" + op2)
    }
    def oneOp(name: String, op: Int): Unit = {
      println(name + " $" + op)
    }
    def iFormat(name: String, op1: Int, op2: Int, i: Int, offset: Boolean): Unit = {
      if (offset) println(name + " $" + op1 + ", " + i + "($" + op2 + ")")
      else println(name + " $" + op1 + ", $" + op2 + ", " + i)
    }

    def word(i: Int): Unit = {
      println(".word " + i.toHexString)
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
      this.oneOp("mfhi", op)
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
    def bne(op1: Int, op2: Int, i: Int): Unit = {
      this.iFormat("bne", op1, op2, i, offset = false)
    }
    def jr(op: Int): Unit = {
      this.oneOp("jr", op)
    }
    def jalr(op: Int): Unit = {
      this.oneOp("jalr", op)
    }
    def num(value: Int): Int = {
      if (value == 0) 0
      else {
        lis(3)
        word(value)
        3
      }
    }
  }
}
