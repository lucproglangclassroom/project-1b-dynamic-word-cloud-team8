import scala.io.Source
import scala.util.Try

object WordCount {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage: WordCount <filename>")
      sys.exit(1)
    }

    val filename = args(0)
    
    val wordCountMap = countWords(filename)
    
    wordCountMap match {
      case Some(counts) => println(counts)
      case None => println(s"Error: Unable to process file '$filename'")
    }
  }

  def countWords(filename: String): Option[Map[String, Int]] = {
    Try {
      val source = Source.fromFile(filename)
      try {
        source.getLines()
          .flatMap(_.split("\\s+"))
          .map(_.toLowerCase)
          .foldLeft(Map.empty[String, Int]) { (map, word) =>
            map + (word -> (map.getOrElse(word, 0) + 1))
          }
      } finally {
        source.close()
      }
    }.toOption
  }
}