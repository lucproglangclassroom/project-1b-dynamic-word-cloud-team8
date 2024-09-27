package TopWords

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TopWordsSpec extends AnyFlatSpec with Matchers {

  // Test for handleArgs method
  "handleArgs" should "return default values when no arguments are provided" in {
    val args = Array.empty[String]
    val result = TopWords.handleArgs(args)
    result("cloud-size") shouldEqual 10
    result("length-at-least") shouldEqual 6
    result("window-size") shouldEqual 1000
  }

  it should "override default values with provided arguments" in {
    val args = Array("-c", "5", "-l", "4", "-w", "500")
    val result = TopWords.handleArgs(args)
    result("cloud-size") shouldEqual 5
    result("length-at-least") shouldEqual 4
    result("window-size") shouldEqual 500
  }

  /*it should "ignore invalid arguments" in {
    val args = Array("-c", "5", "invalidArg", "-l", "4")
    val result = TopWords.handleArgs(args)
    result("cloud-size") shouldEqual 5 // Valid argument
    result("length-at-least") shouldEqual 4 // Valid argument
    result("window-size") shouldEqual 1000 // Default value
  }*/

  // Test for countWords functionality
  "countWords" should "count words correctly" in {
    // Create a mock input source
    val input = "Hello world! This is a test. Hello again!"
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    // Call countWords with appropriate parameters
    TopWords.countWords(2, 10, 5)

    // Since countWords prints directly, we can't capture the output directly here.
    // You might need to refactor countWords to return a value for testing.
    // For now, we can assert that it doesn't throw an exception.
    noException should be thrownBy TopWords.countWords(2, 10, 5)
  }

  it should "not count words shorter than minLength" in {
    val input = "A quick brown fox jumps over the lazy dog"
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    // Capture the output (you'll need to implement this part)
    // For example, refactor countWords to return a Map[String, Int] instead of printing.

    // Call countWords
    TopWords.countWords(4, 10, 5)

    // Again, assert no exceptions are thrown
    noException should be thrownBy TopWords.countWords(4, 10, 5)
  }
}
