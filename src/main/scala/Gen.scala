class Gen {

  class Frame(var scope: WLP4Gen.Procedure) {
    this.scope = scope
    val fp: Int = 29
    val sp: Int = 30
    val orgRtn: Int = 31
    val wain: Boolean = this.scope.name == "wain"

    // TODO : By the book
    def prolog(): Unit = {
      MIPS.sub(fp, sp, 4)
      MIPS.sub(sp, sp, Code.num(this.scope.scope.length))
      this.push(null, -1)

      if (this.wain) {
        MIPS.sw(1, fp, this.scope.params.scope.head.address + 4)
        MIPS.sw(2, fp, this.scope.params.scope(1).address + 4)
      }
    }
    // TODO : By the book
    def epilog(): Unit = {
      MIPS.add(sp, fp, 4)
      MIPS.lw(orgRtn, fp, 0)
    }

    def body(): Unit = {
      if (!this.wain) {
        // TODO : Add params to frame
        println()
      }
      // TODO : Add vars to frame

      // TODO : CODE GEN
    }

    def push(id: String, value: Int): Unit = {
      if (id == null && value == -1) MIPS.sw(orgRtn, fp, 0)
      MIPS.sw(Code.num(value), fp, this.scope.get(id).address + 4)
    }
  }

  object Code {

    def num(value: Int): Int = {
      if (value == 0) 0
      else {
        MIPS.lis(3)
        MIPS.word(value)
        3
      }
    }

  }


}
