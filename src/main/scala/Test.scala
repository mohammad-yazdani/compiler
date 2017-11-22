import java.io.File


object Test {

  val dirName: String = "src/main/scala/"
  val testFiles: Seq[String] = new File(dirName).list().toSeq

  def main(args: Array[String]): Unit = {
    this.testFiles.find(file => file.endsWith(".wlp4i"))
      .foreach(file => WLP4Gen.main(Array[String](dirName + file)))
  }
}
