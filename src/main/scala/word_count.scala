import scala.io.Source
import scala.util.Try
import scala.language.unsafeNulls
import scala.language.postfixOps

object WordCount{
  def handleArgs(args: Array[String]): List[String] = {
    // Default values
    val defaults = Map(
      "cloud-size" -> 10,
      "length-at-least" -> 6,
      "window-size" -> 1000
    )

    // Collect arguments from the passed array
    val collectedArgs = args.grouped(2).collect {
      case Array("-c", value) => "cloud-size" -> value
      case Array("--cloud-size", value) => "cloud-size" -> value
      case Array("-l", value) => "length-at-least" -> value
      case Array("--length-at-least", value) => "length-at-least" -> value
      case Array("-w", value) => "window-size" -> value
      case Array("--window-size", value) => "window-size" -> value
    }.toList

    // Create a map with defaults and any provided arguments
    val argMap = collectedArgs.toMap.withDefault(identity)

    // Create the final argument map combining defaults and provided values
    val finalArgs = Map(
      "cloud-size" -> argMap.getOrElse("cloud-size", defaults("cloud-size").toString),
      "length-at-least" -> argMap.getOrElse("length-at-least", defaults("length-at-least").toString),
      "window-size" -> argMap.getOrElse("window-size", defaults("window-size").toString)
    )

    // Create result list to return
    finalArgs.toList.map {
      case (key, value) => s"$key: $value"
    }
}
  def main(args: Array[String]): Unit = {

    val word_cloud_args = handleArgs(args)
    if (args.length < 1) {
      println("Usage: WordCount <filename>") //Change based on desired input
      sys.exit(1)
    }

    val filename = args(0) //Get the filename from args
    
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

  def readIgnoreList(filePath: String): Option[Set[String]] = {
    val ignoreList = Source.fromFile(filePath).getLines()
    val filteredMap = ignoreList.flatMap(line => Option(line.trim).map(_.toString))
    Some(filteredMap.toSet)
  }

  def formatWordCountForOutput(processedData: Map[String, Int], ignoreList: Option[Set[String]]): String = {
    val filteredData = ignoreList match {
      case Some(list) => processedData.filterNot { case (word, _) => list.contains(word) }
      case None => processedData
    }

    val wordCloud = filteredData.toSeq
      .sortBy { case (word, count) => (-count, word) }
    
    wordCloud.map { case (word, count) => s"$word: $count" }
      .mkString(" ")
  }
}

