import scala.io.Source
import scala.language.postfixOps

class MapToCloudWithIgnore {

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
