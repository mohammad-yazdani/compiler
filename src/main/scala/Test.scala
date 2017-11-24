import java.io.File


object Test {

  val dirName: String = "src/main/scala/"
  val testFiles: Seq[String] = new File(dirName).list().toSeq

  def main(args: Array[String]): Unit = {
    val tests: Seq[String] = this.testFiles.filter(file => file.endsWith(".wlp4i"))
    tests.foreach(file => {
      System.err.println(file)
      WLP4Gen.main(Array[String](dirName + file))
    })
  }
}
