object MIPS {

  // TODO : Store 4 in $4
  this.lis(4)
  this.word(4)

  // TODO : Store 1 in $11
  this.lis(11)
  this.word(1)

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