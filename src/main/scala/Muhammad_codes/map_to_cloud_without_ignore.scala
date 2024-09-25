class map_to_cloud_without_ignore {
  def formatWordCountForOutput(processedData: Map[String, Int]): String = {
    // Sort by frequency
    val wordCloud = processedData.toSeq
      // sort alphabetically
      .sortBy { case (word, count) => (-count, word) }

    // Format output following 1B instructions
    wordCloud.map { case (word, count) => s"$word: $count" }
      .mkString(" ")
  }
}
