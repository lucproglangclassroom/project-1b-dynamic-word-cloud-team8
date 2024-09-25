package Muhammad_codes

import scala.io.Source

class map_to_cloud_w_ignore {

  // Method to read the ignore list from a file
  def readIgnoreList(filePath: String): Set[String] = {
    val ignoreList = Source.fromFile(filePath).getLines().map(_.toLowerCase).filter(_ != null) .toSet

    ignoreList
  }

  // Method to format word counts for output
  def formatWordCountForOutput(processedData: Map[String, Int], ignoreList: Set[String]): String = {
    // Process data
    val wordCloud = processedData.toSeq
      // filter words in list
      .filterNot { case (word, _) => ignoreList.contains(word.toLowerCase) }
      // sort alphabetically and by frequency
      .sortBy { case (word, count) => (-count, word) }

    // Create a formatted string in the desired "word: count" format
    wordCloud.map { case (word, count) => s"$word: $count" }.mkString(" ")
  }
}


