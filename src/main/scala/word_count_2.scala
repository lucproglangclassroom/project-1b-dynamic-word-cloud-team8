import scala.io.Source
import scala.collection.mutable.Queue
import scala.util.control.Breaks._
import scala.sys.process._
import scala.util.Try
import scala.language.unsafeNulls

object TopWords {

  // Handles command-line arguments and returns a map with the parsed arguments
  def handleArgs(args: Array[String]): Map[String, Int] = {
    // Default values
    val defaults = Map(
      "cloud-size" -> 10,
      "length-at-least" -> 6,
      "window-size" -> 1000
    )

    // Collect arguments from the passed array
    val collectedArgs = args.grouped(2).collect {
      case Array("-c", value) => "cloud-size" -> value.toInt
      case Array("--cloud-size", value) => "cloud-size" -> value.toInt
      case Array("-l", value) => "length-at-least" -> value.toInt
      case Array("--length-at-least", value) => "length-at-least" -> value.toInt
      case Array("-w", value) => "window-size" -> value.toInt
      case Array("--window-size", value) => "window-size" -> value.toInt
    }.toMap

    // Combine default values with the provided arguments
    defaults ++ collectedArgs
  }

  def main(args: Array[String]): Unit = {
    // Handle arguments
    val wordCloudArgs = handleArgs(args)

    val cloudSize = wordCloudArgs("cloud-size")
    val minLength = wordCloudArgs("length-at-least")
    val windowSize = wordCloudArgs("window-size")

    // Read from standard input
    println(s"Processing words with cloudSize: $cloudSize, minLength: $minLength, windowSize: $windowSize")

    countWords(minLength, windowSize, cloudSize)
  }

  // Modified countWords to process a continuous stream of input and maintain a sliding window
  def countWords(minLength: Int, windowSize: Int, cloudSize: Int): Unit = {
    val wordQueue = Queue[String]()   // Sliding window of words
    var wordCountMap = Map[String, Int]()  // Word frequency map

    breakable {
      val lines = Source.stdin.getLines()
      val words = lines.flatMap(l => l.split("(?U)[^\\p{Alpha}0-9']+"))
      
      words.foreach { word =>
        if (word != null) {
          val cleanedWord = word.toLowerCase().trim

          if (cleanedWord.length >= minLength) {
            // Add word to the sliding window
            wordQueue.enqueue(cleanedWord)
            wordCountMap = wordCountMap + (cleanedWord -> (wordCountMap.getOrElse(cleanedWord, 0) + 1))

            // Remove oldest word if window exceeds size
            if (wordQueue.size > windowSize) {
              val oldestWord = wordQueue.dequeue()
              wordCountMap = wordCountMap + (oldestWord -> (wordCountMap(oldestWord) - 1))
              if (wordCountMap(oldestWord) == 0) {
                wordCountMap -= oldestWord
              }
            }

            // Print word cloud when window has enough words
            if (wordQueue.size >= windowSize) {
              printWordCloud(wordCountMap, cloudSize)
            }
          }
        }
      }
      // Handle SIGPIPE by breaking out of the loop if output is closed
      if (isStdoutClosed) {
          break
      }
    }
  }

  // Function to print the top words in the word cloud
  def printWordCloud(wordCountMap: Map[String, Int], cloudSize: Int): Unit = {
    // Sort words by frequency (descending), then alphabetically, and limit to cloudSize
    val sortedWords = wordCountMap.toSeq
      .sortBy { case (word, count) => (-count, word) }
      .take(cloudSize)

    val wordCloud = sortedWords.map { case (word, count) => s"$word: $count" }
    println(wordCloud.mkString(" "))
  }

  // Utility function to check if stdout is closed (for SIGPIPE handling)
  def isStdoutClosed: Boolean = {
    Try(System.out.nn.flush()).isFailure
  }
}
