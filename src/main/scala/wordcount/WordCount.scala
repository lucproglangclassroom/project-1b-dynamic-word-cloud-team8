package TopWords

import scala.io.Source
import scala.collection.mutable.Queue
import scala.util.control.Breaks._
import scala.sys.process._
import scala.util.Try
import scala.language.unsafeNulls
import com.typesafe.scalalogging.LazyLogging

object TopWords extends LazyLogging {

  def handleArgs(args: Array[String]): Map[String, Int] = {
    logger.info("Handling command-line arguments")

    val defaults = Map(
      "cloud-size" -> 10,
      "length-at-least" -> 6,
      "window-size" -> 1000
    )

    val collectedArgs = args.grouped(2).collect {
      case Array("-c", value) =>
        logger.debug(s"Parsed cloud-size: $value")
        "cloud-size" -> value.toInt
      case Array("--cloud-size", value) =>
        logger.debug(s"Parsed cloud-size: $value")
        "cloud-size" -> value.toInt
      case Array("-l", value) =>
        logger.debug(s"Parsed length-at-least: $value")
        "length-at-least" -> value.toInt
      case Array("--length-at-least", value) =>
        logger.debug(s"Parsed length-at-least: $value")
        "length-at-least" -> value.toInt
      case Array("-w", value) =>
        logger.debug(s"Parsed window-size: $value")
        "window-size" -> value.toInt
      case Array("--window-size", value) =>
        logger.debug(s"Parsed window-size: $value")
        "window-size" -> value.toInt
    }.toMap

    val finalArgs = defaults ++ collectedArgs
    logger.info(s"Final arguments: $finalArgs")
    finalArgs
  }

  def main(args: Array[String]): Unit = {
    logger.info("Application started")
    val wordCloudArgs = handleArgs(args)

    val cloudSize = wordCloudArgs("cloud-size")
    val minLength = wordCloudArgs("length-at-least")
    val windowSize = wordCloudArgs("window-size")

    logger.info(s"Processing words with cloudSize: $cloudSize, minLength: $minLength, windowSize: $windowSize")

    countWords(minLength, windowSize, cloudSize)
  }

  def countWords(minLength: Int, windowSize: Int, cloudSize: Int): Unit = {
    val wordQueue = Queue[String]()
    var wordCountMap = Map[String, Int]()

    breakable {
      val lines = Source.stdin.getLines()
      val words = lines.flatMap(l => l.split("(?U)[^\\p{Alpha}0-9']+"))

      words.foreach { word =>
        val cleanedWord = word.toLowerCase().trim
        if (cleanedWord.length >= minLength) {
          wordQueue.enqueue(cleanedWord)
          wordCountMap = wordCountMap + (cleanedWord -> (wordCountMap.getOrElse(cleanedWord, 0) + 1))
          logger.debug(s"Added word: $cleanedWord, updated count: ${wordCountMap(cleanedWord)}")

          if (wordQueue.size > windowSize) {
            val oldestWord = wordQueue.dequeue()
            wordCountMap = wordCountMap + (oldestWord -> (wordCountMap(oldestWord) - 1))
            logger.debug(s"Removed oldest word: $oldestWord, updated count: ${wordCountMap.getOrElse(oldestWord, 0)}")
            if (wordCountMap(oldestWord) == 0) {
              wordCountMap -= oldestWord
              logger.debug(s"Word $oldestWord removed from wordCountMap")
            }
          }

          if (wordQueue.size >= windowSize) {
            logger.info("Printing word cloud")
            printWordCloud(wordCountMap, cloudSize)
          }
        }
      }

      if (isStdoutClosed) {
        logger.warn("Standard output is closed. Breaking out of the loop.")
        break
      }
    }
  }

  def printWordCloud(wordCountMap: Map[String, Int], cloudSize: Int): Unit = {
    val sortedWords = wordCountMap.toSeq
      .sortBy { case (word, count) => (-count, word) }
      .take(cloudSize)

    val wordCloud = sortedWords.map { case (word, count) => s"$word: $count" }
    println(wordCloud.mkString(" "))
  }

  def isStdoutClosed: Boolean = {
    val isClosed = Try(System.out.nn.flush()).isFailure
    if (isClosed) {
      logger.warn("Standard output seems to be closed.")
    }
    isClosed
  }
}
