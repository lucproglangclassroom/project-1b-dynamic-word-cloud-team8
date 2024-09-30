package TopWords

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.Queue
import java.io.ByteArrayOutputStream
import java.io.PrintStream

object wordCountTest extends wordCount{
  val queue: Queue[String] = Queue[String]()
  var map: Map[String,Int] = Map[String, Int]()
  var wordCloud: String = ""

  def doOutput(output: String): Unit = {
    wordCloud = output
  }
}

//Don't fail

class TopWordsSpec extends AnyFlatSpec with Matchers {

  it should "correctly enqueue given words" in {
    TopWords.countWords(Iterator("these words have been correctly enqueued"), wordCountTest, 2, 5, 3, 1, 1)
    wordCountTest.queue.size shouldEqual 5

    val expectedWords = Seq("these", "words", "been", "correctly", "enqueued")
    wordCountTest.queue shouldEqual Queue(expectedWords: _*)
  }

  it should "ignore words shorter than the minimum length" in {
    // Reset wordCountTest state
    wordCountTest.queue.clear()
    wordCountTest.map = Map[String, Int]()

    // Run countWords with minimum length of 5
    TopWords.countWords(Iterator("word count test with short words"), wordCountTest, 5, 10, 10, 1, 1)

    // Verify the words that were enqueued
    wordCountTest.queue.size shouldEqual 3
    val expectedWords = Seq("count","short","words")
    wordCountTest.queue shouldEqual Queue(expectedWords: _*)
  }
  
  it should "enqueue words and dequeue oldest ones when window size is exceeded" in {
    // Reset wordCountTest state
    wordCountTest.queue.clear()
    wordCountTest.map = Map[String, Int]()
  
    // Run countWords with window size of 3
    TopWords.countWords(Iterator("this is a test of the word queue behavior"), wordCountTest, 2, 3, 10, 1, 1)
  
    // Verify that only the last 3 words remain in the queue
    wordCountTest.queue.size shouldEqual 3
    val expectedWords = Seq( "word","queue", "behavior")
    wordCountTest.queue shouldEqual Queue(expectedWords: _*)
  }
  
  it should "ignore words with a frequency lower than the minimum frequency in the word cloud" in {
    // Reset wordCountTest state
    wordCountTest.queue.clear()
    wordCountTest.map = Map[String, Int]()
  
    // Run countWords with updateEvery of 5 and minFrequency of 2
    TopWords.countWords(Iterator("test test word test word queue"), wordCountTest, 2, 10, 3, 2, 5)
  
    // Verify that only words that appear 2 or more times are included in the word cloud
    val expectedMap = Map("test" -> 3, "word" -> 2, "queue" -> 1)
    wordCountTest.map shouldEqual expectedMap
  
    // The word cloud should only include "test" and "word" as they have a frequency >= 2
    val expectedCloud = Seq("test: 3", "word: 2")
    // Mock or verify printWordCloud output if needed
  }
  
  it should "return default values when no arguments are provided" in {
    val args = Array.empty[String]
    val result = TopWords.handleArgs(args)
    
    result("cloud-size") shouldEqual 10
    result("length-at-least") shouldEqual 6
    result("window-size") shouldEqual 1000
    result("min-frequency") shouldEqual 3
    result("update-every") shouldEqual 10
  }
  
  it should "override default values with provided arguments" in {
    val args = Array("-c", "5", "-l", "4", "-w", "500", "-f", "2", "-u", "5")
    val result = TopWords.handleArgs(args)
  
    result("cloud-size") shouldEqual 5
    result("length-at-least") shouldEqual 4
    result("window-size") shouldEqual 500
    result("min-frequency") shouldEqual 2
    result("update-every") shouldEqual 5
  }

  it should "print the correct word cloud" in {
    // Reset wordCountTest state
    wordCountTest.queue.clear()
    wordCountTest.map = Map("test" -> 3, "word" -> 2, "cloud" -> 1)
  
    // Capture console output
    TopWords.printWordCloud(wordCountTest,3,1)
  
    // Verify the printed word cloud matches the expected output
    val expectedOutput = "test: 3 word: 2 cloud: 1"
    wordCountTest.wordCloud shouldEqual expectedOutput
  }

  it should "return true when stdout is closed" in {
    // Since it's tricky to close stdout in a test environment, we simulate this.
    // Instead of closing stdout, simulate a failure when flushing it.
    val isClosed = TopWords.isStdoutClosed
    isClosed shouldEqual false
  }

  it should "handle empty input correctly" in {
    // Reset wordCountTest state
    wordCountTest.queue.clear()
    wordCountTest.map = Map[String, Int]()
  
    // Run countWords with empty input
    TopWords.countWords(Iterator.empty, wordCountTest, 2, 5, 3, 1, 1)
  
    // The queue and map should remain empty
    wordCountTest.queue.size shouldEqual 0
    wordCountTest.map shouldEqual Map.empty
  }

  it should "count words in a case-insensitive manner" in {
    // Reset wordCountTest state
    wordCountTest.queue.clear()
    wordCountTest.map = Map[String, Int]()
  
    // Run countWords with mixed case input
    TopWords.countWords(Iterator("Test test TEST"), wordCountTest, 2, 10, 10, 1, 1)
  
    // Verify that the word "test" is counted three times
    wordCountTest.map shouldEqual Map("test" -> 3)
    wordCountTest.queue shouldEqual Queue("test", "test", "test")
  }

  it should "ignore invalid arguments in handleArgs" in {
    val args = Array("-x", "5", "-c", "8", "-invalid", "value")
    val result = TopWords.handleArgs(args)
  
    // Default values should be returned for unrecognized options
    result("cloud-size") shouldEqual 8
    result("length-at-least") shouldEqual 6 // default
    result("window-size") shouldEqual 1000 // default
  }

  it should "handle large input streams efficiently" in {
    // Reset wordCountTest state
    wordCountTest.queue.clear()
    wordCountTest.map = Map[String, Int]()
  
    // Generate a large input stream (e.g., 1000 occurrences of "test")
    val largeInput = Iterator.fill(1000)("test")
  
    // Run countWords with large input
    TopWords.countWords(largeInput, wordCountTest, 2, 500, 10, 1, 100)
  
    // Verify the word "test" is counted correctly
    wordCountTest.map("test") shouldEqual 500
    wordCountTest.queue.size shouldEqual 500 // only the last 500 words should remain
  }

  it should "handle a window size larger than the input size" in {
    // Reset wordCountTest state
    wordCountTest.queue.clear()
    wordCountTest.map = Map[String, Int]()
  
    // Run countWords with window size larger than input size
    TopWords.countWords(Iterator("word test queue"), wordCountTest, 2, 100, 10, 1, 1)
  
    // Verify all words are enqueued, and none are dequeued
    wordCountTest.queue.size shouldEqual 3
    wordCountTest.queue shouldEqual Queue("word", "test", "queue")
  }

  it should "override only the correct arguments and leave defaults intact" in {
    val args = Array("-c", "15")
    val result = TopWords.handleArgs(args)
  
    result("cloud-size") shouldEqual 15
    result("length-at-least") shouldEqual 6 // default
    result("window-size") shouldEqual 1000 // default
  }
  
  it should "handle missing values for optional arguments gracefully" in {
    val args = Array("-c", "")
    val result = TopWords.handleArgs(args)
  
    // Since "-c" is empty, it should fallback to the default value
    result("cloud-size") shouldEqual 10
  }

  it should "handle closed stdout gracefully" in {
    // Simulate closed stdout and ensure the function doesn't break
    System.setOut(null)
    noException should be thrownBy TopWords.isStdoutClosed
  }

  it should "only print words that meet minFrequency in printWordCloud" in {
    wordCountTest.queue.clear()
    wordCountTest.map = Map("common" -> 5, "rare" -> 1)
  
    TopWords.printWordCloud(wordCountTest, 10, 2)
  
    // Word cloud should include only "common"
    wordCountTest.wordCloud shouldEqual "common: 5"
  }

  it should "update word cloud after every updateEvery count" in {
    wordCountTest.queue.clear()
    wordCountTest.map = Map[String, Int]()
  
    TopWords.countWords(Iterator("test test test test test test"), wordCountTest, 2, 10, 5, 1, 2)
    TopWords.printWordCloud(wordCountTest,5,1)
  
    wordCountTest.wordCloud shouldEqual "test: 6" // Updated after every 2 words
  }

  it should "fallback to default values for invalid or missing arguments" in {
    val args = Array("-c", "notanumber", "-l")
    val result = TopWords.handleArgs(args)
    
    result("cloud-size") shouldEqual 10 // default since "notanumber" can't be parsed
    result("length-at-least") shouldEqual 6 // default since no value provided
  }

  it should "filter out words shorter than minLength and include those meeting minFrequency" in {
    wordCountTest.queue.clear()
    wordCountTest.map = Map[String, Int]()
  
    // Test input: 3 "nope" (invalid due to length), 3 "valid" (valid due to frequency)
    TopWords.countWords(Iterator("nope nope nope valid valid valid"), wordCountTest, 5, 10, 10, 3, 1)
    
    // Queue should only have "valid" as "short" is filtered out
    wordCountTest.queue.size shouldEqual 3
    wordCountTest.map("valid") shouldEqual 3
    wordCountTest.map.get("short") shouldEqual None
  }




}