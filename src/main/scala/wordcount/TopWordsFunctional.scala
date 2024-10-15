package TopWordsFunctional

import javafx.application.Platform
import wordcount.wordCloudVisualizer
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future
import scala.io.Source
import scala.util.Try
import scala.language.unsafeNulls

// Trait for handling configuration parameters
trait Config {
  def handleArgs(args: Array[String]): Map[String, Int] = {
    val defaults = Map(
      "cloud-size" -> 10,
      "length-at-least" -> 6,
      "window-size" -> 1000,
      "min-frequency" -> 3,
      "update-every" -> 10
    )

    def safeParseInt(value: String, default: Int): Int =
      Try(value.toInt).getOrElse(default)

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

      // Flags without values
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
}

// Trait for word processing logic
trait WordProcessor {
  def processWords(
      words: Iterator[String],
      ignoredWords: Set[String],
      minLength: Int,
      windowSize: Int,
      updateEvery: Int
  ): Iterator[(Map[String, Int], Int)] = {
    words
      .map(_.toLowerCase.trim)
      .filter(word => word.length >= minLength && !ignoredWords.contains(word))
      .scanLeft[(List[String], Map[String, Int], Int)]((List.empty[String], Map.empty[String, Int], 0)) {
        case ((window, counts, counter), word) =>
          // Update window
          val newWindow = (word :: window).take(windowSize)
          // Update counts
          val updatedCounts = counts + (word -> (counts.getOrElse(word, 0) + 1))
          if (window.size >= windowSize) {
            val removedWord = window.last
            val newCount = updatedCounts(removedWord) - 1
            val finalCounts = if (newCount == 0) updatedCounts - removedWord else updatedCounts + (removedWord -> newCount)
            (newWindow, finalCounts, counter + 1)
          } else {
            (newWindow, updatedCounts, counter + 1)
          }
      }
      .drop(1) // Remove the initial seed
      .filter { case (window, _, counter) =>
        val shouldUpdate = window.size == windowSize && counter % updateEvery == 0
        if (shouldUpdate) println(s"[processWords] Counter $counter reached and window is full. Preparing to output word cloud.")
        shouldUpdate
      }
      .map { case (_, counts, _) => (counts, counts.size) }
  }
}

// Trait for output handling
trait OutputHandler {
  def doOutput(output: String): Unit = {
    println(output)
  }

  def printWordCloud(wordCounts: Map[String, Int], cloudSize: Int, minFrequency: Int): String = {
    wordCounts
      .filter { case (_, count) => count >= minFrequency }
      .toSeq
      .sortBy { case (word, count) => (-count, word) }
      .take(cloudSize)
      .map { case (word, count) => s"$word: $count" }
      .mkString(" ")
  }
}

// Main object combining all traits
object TopWordsFunctional extends Config with WordProcessor with OutputHandler {

  def main(args: Array[String]): Unit = {
    val wordCloudArgs = handleArgs(args)

    val cloudSize = wordCloudArgs("cloud-size")
    val minLength = wordCloudArgs("length-at-least")
    val windowSize = wordCloudArgs("window-size")
    val minFrequency = wordCloudArgs("min-frequency")
    val updateEvery = wordCloudArgs("update-every")

    println(s"[main] DEBUG TopWordsFunctional - cloudSize=$cloudSize, minLength=$minLength, windowSize=$windowSize, updateEvery=$updateEvery, minFrequency=$minFrequency")

    // Read and process ignored words
    val ignoredWords = try {
      Source.fromFile("ignore-list").getLines().map(_.trim).filter(_.nonEmpty).toSet
    } catch {
      case e: Exception =>
        println(s"[main] ERROR: Could not read 'ignore-list' file. ${e.getMessage}")
        Set.empty[String]
    }
    println(s"[main] DEBUG Ignored Words: ${ignoredWords.mkString(", ")}")

    // Corrected regex: [^\p{Alpha}0-9']+
    val words = Source.stdin.getLines().flatMap(line => line.split("""(?U)[^\p{Alpha}0-9']+"""))


    Future {
      wordCloudVisualizer.main(Array.empty) // Start the JavaFX application
    }

    // Process words and obtain iterator of word counts
    val processed = processWords(words, ignoredWords, minLength, windowSize, updateEvery)

    // Debugging: Check if `processed` has elements
    if (processed.hasNext) {
      println("[main] DEBUG: Processing word counts...")
    } else {
      println("[main] DEBUG: No word counts to process. Check if input has enough words and that filters are correct.")
    }

    // Iterate over processed word counts and print the word cloud
    processed.foreach { case (counts, _) =>
      val wordCloud = printWordCloud(counts, cloudSize, minFrequency)
      doOutput(wordCloud)
      Thread.sleep(1000) // Adjust the time as needed
      Platform.runLater(() => {
        wordCloudVisualizer.getInstance().updateWordCloud(wordCloud)
      })

    }
  }
}
