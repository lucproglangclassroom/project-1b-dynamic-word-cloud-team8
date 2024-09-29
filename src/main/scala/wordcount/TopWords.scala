package TopWords

import scala.io.Source
import scala.collection.mutable.Queue
import scala.util.control.Breaks._
import scala.sys.process._
import scala.util.Try
import scala.language.unsafeNulls

object TopWords {

  def handleArgs(args: Array[String]): Map[String, Int] = {
    val defaults = Map(
      "cloud-size" -> 10,
      "length-at-least" -> 6,
      "window-size" -> 1000,
      "min-frequency" -> 3,
      "update-every" -> 10
    )

    val collectedArgs = args.grouped(2).collect {
      case Array("-c", value) => "cloud-size" -> value.toInt
      case Array("--cloud-size", value) => "cloud-size" -> value.toInt
      case Array("-l", value) => "length-at-least" -> value.toInt
      case Array("--length-at-least", value) => "length-at-least" -> value.toInt
      case Array("-w", value) => "window-size" -> value.toInt
      case Array("--window-size", value) => "window-size" -> value.toInt
      case Array("-f", value) => "min-frequency" -> value.toInt
      case Array("--min-frequency", value) => "min-frequency" -> value.toInt
      case Array("-u", value) => "update-every" -> value.toInt
      case Array("--update-every", value) => "update-every" -> value.toInt
    }.toMap

    defaults ++ collectedArgs
  }

  def main(args: Array[String]): Unit = {
    val wordCloudArgs = handleArgs(args)

    val cloudSize = wordCloudArgs("cloud-size")
    val minLength = wordCloudArgs("length-at-least")
    val windowSize = wordCloudArgs("window-size")
    val minFrequency = wordCloudArgs("min-frequency")
    val updateEvery = wordCloudArgs("update-every")

    println(s"[main] DEBUG edu.luc.cs.cs371.topwords.Main - howMany=$cloudSize minLength=$minLength lastNWords=$windowSize everyKSteps=$updateEvery minFrequency=$minFrequency")

    countWords(minLength, windowSize, cloudSize, minFrequency, updateEvery)
  }

  def countWords(minLength: Int, windowSize: Int, cloudSize: Int, minFrequency: Int, updateEvery: Int): Unit = {
    val wordQueue = Queue[String]()
    var wordCountMap = Map[String, Int]()
    var wordCounter = 0

    breakable {
      val lines = Source.stdin.getLines()
      val words = lines.flatMap(l => l.split("(?U)[^\\p{Alpha}0-9']+"))

      words.foreach { word =>
        val cleanedWord = word.toLowerCase().trim
        if (cleanedWord.length >= minLength) {
          wordQueue.enqueue(cleanedWord)
          wordCountMap = wordCountMap + (cleanedWord -> (wordCountMap.getOrElse(cleanedWord, 0) + 1))
          wordCounter += 1

          if (wordQueue.size > windowSize) {
            val oldestWord = wordQueue.dequeue()
            wordCountMap = wordCountMap + (oldestWord -> (wordCountMap(oldestWord) - 1))
            if (wordCountMap(oldestWord) == 0) {
              wordCountMap -= oldestWord
            }
          }

          if (wordCounter % updateEvery == 0) {
            printWordCloud(wordCountMap, cloudSize, minFrequency)
          }
        }
      }

      if (isStdoutClosed) {
        break
      }
    }
  }

  def printWordCloud(wordCountMap: Map[String, Int], cloudSize: Int, minFrequency: Int): Unit = {
    val sortedWords = wordCountMap.toSeq
      .filter { case (_, count) => count >= minFrequency }
      .sortBy { case (word, count) => (-count, word) }
      .take(cloudSize)

    val wordCloud = sortedWords.map { case (word, count) => s"$word: $count" }
    println(wordCloud.mkString(" "))
  }

  def isStdoutClosed: Boolean = {
    Try(System.out.nn.flush()).isFailure
  }
}
