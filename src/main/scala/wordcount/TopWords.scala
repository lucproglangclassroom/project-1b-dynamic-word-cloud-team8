package TopWords

import scala.io.Source
import scala.collection.mutable.Queue
import scala.util.control.Breaks._
import scala.sys.process._
import scala.util.Try
import scala.language.unsafeNulls

trait wordCount {
  val queue : Queue[String]
  var map : Map[String, Int]
}

object wordCountMain extends wordCount{
  val queue: Queue[String] = Queue[String]()
  var map: Map[String,Int] = Map[String, Int]()
}

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


    countWords(Source.stdin.getLines(), wordCountMain, minLength, windowSize, cloudSize, minFrequency, updateEvery)
  }

  def countWords(lines: Iterator[String] , word_count: wordCount, minLength: Int, windowSize: Int, cloudSize: Int, minFrequency: Int, updateEvery: Int): Unit = {
    var wordCounter = 0

    breakable {
      val words = lines.flatMap(l => l.split("(?U)[^\\p{Alpha}0-9']+"))

      words.foreach { word =>
        val cleanedWord = word.toLowerCase().trim
        if (cleanedWord.length >= minLength) {
          word_count.queue.enqueue(cleanedWord)
          word_count.map = word_count.map + (cleanedWord -> (word_count.map.getOrElse(cleanedWord, 0) + 1))
          wordCounter += 1

          if (word_count.queue.size > windowSize) {
            val oldestWord = word_count.queue.dequeue()
            word_count.map = word_count.map + (oldestWord -> (word_count.map(oldestWord) - 1))
            if (word_count.map(oldestWord) == 0) {
              word_count.map -= oldestWord
            }
          }

          if (wordCounter % updateEvery == 0 && word_count.queue.size == windowSize) {
            printWordCloud(word_count, cloudSize, minFrequency)
          }
        }
      }

      if (isStdoutClosed) {
        break
      }
    }
  }

  def printWordCloud(word_count: wordCount, cloudSize: Int, minFrequency: Int): Unit = {
    val sortedWords = word_count.map.toSeq
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
