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

  def doOutput(output: String): Unit
  
}

object wordCountMain extends wordCount{
  val queue: Queue[String] = Queue[String]()
  var map: Map[String,Int] = Map[String, Int]()

  def doOutput(output: String): Unit = {
    println(output)
  }
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
  
    // Safely parse integers and fall back to defaults if parsing fails
    def safeParseInt(value: String, default: Int): Int = {
      Try(value.toInt).getOrElse(default)
    }
  
    val collectedArgs = args.grouped(2).collect {
      case Array("-c", value) => "cloud-size" -> safeParseInt(value, defaults("cloud-size"))
      case Array("--cloud-size", value) => "cloud-size" -> safeParseInt(value, defaults("cloud-size"))
      case Array("-l", value) => "length-at-least" -> safeParseInt(value, defaults("length-at-least"))
      case Array("--length-at-least", value) => "length-at-least" -> safeParseInt(value, defaults("length-at-least"))
      case Array("-w", value) => "window-size" -> safeParseInt(value, defaults("window-size"))
      case Array("--window-size", value) => "window-size" -> safeParseInt(value, defaults("window-size"))
      case Array("-f", value) => "min-frequency" -> safeParseInt(value, defaults("min-frequency"))
      case Array("--min-frequency", value) => "min-frequency" -> safeParseInt(value, defaults("min-frequency"))
      case Array("-u", value) => "update-every" -> safeParseInt(value, defaults("update-every"))
      case Array("--update-every", value) => "update-every" -> safeParseInt(value, defaults("update-every"))
      
      // In case a flag is provided without a value, use the default
      case Array("-c") => "cloud-size" -> defaults("cloud-size")
      case Array("--cloud-size") => "cloud-size" -> defaults("cloud-size")
      case Array("-l") => "length-at-least" -> defaults("length-at-least")
      case Array("--length-at-least") => "length-at-least" -> defaults("length-at-least")
      case Array("-w") => "window-size" -> defaults("window-size")
      case Array("--window-size") => "window-size" -> defaults("window-size")
      case Array("-f") => "min-frequency" -> defaults("min-frequency")
      case Array("--min-frequency") => "min-frequency" -> defaults("min-frequency")
      case Array("-u") => "update-every" -> defaults("update-every")
      case Array("--update-every") => "update-every" -> defaults("update-every")
    }.toMap
  
    // Merge default values with collected arguments, giving priority to the parsed args
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
    val ignoredWords = Source.fromFile("ignore-list").getLines().map(_.trim).filter(_.nonEmpty).toSet

    breakable {
      val words = lines.flatMap(l => l.split("(?U)[^\\p{Alpha}0-9']+"))

      words.foreach { word =>
        val cleanedWord = word.toLowerCase().trim
        if (cleanedWord.length >= minLength && !ignoredWords.contains(cleanedWord)) {
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

          if (word_count.queue.size == windowSize && wordCounter % updateEvery == 0) {
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
    word_count.doOutput(wordCloud.mkString(" "))
  }

  def isStdoutClosed: Boolean = {
    Try(System.out.nn.flush()).isFailure
  }
}
