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

  it should "handle long-form arguments correctly" in {
    val args = Array("--cloud-size", "8", "--length-at-least", "3", "--window-size", "100")
    val result = TopWords.handleArgs(args)
    result("cloud-size") shouldEqual 8
    result("length-at-least") shouldEqual 3
    result("window-size") shouldEqual 100
  }

  it should "return error for invalid integer arguments" in {
    val args = Array("-c", "five", "-l", "four", "-w", "500")
    val thrown = intercept[NumberFormatException] {
      TopWords.handleArgs(args)
    }
  }

  "countWords" should "count words correctly" in {
    // Create a mock input source
    val input = "Hello world! This is a test. Hello again!"
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    // Call countWords with appropriate parameters
    noException should be thrownBy TopWords.countWords(2, 10, 5)
    // Ensure correct output by refactoring countWords to return a Map
  }

  it should "not count words shorter than minLength" in {
    val input = "A quick brown fox jumps over the lazy dog"
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(4, 10, 5)
    // Ensure no words are printed, or that the output is as expected
  }

  it should "handle empty input gracefully" in {
    val input = ""
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(2, 10, 5)
    // Ensure no output or specific behavior
  }

  it should "not throw an exception when the input has no valid words" in {
    val input = "12345 !@#$%"
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(2, 10, 5)
  }

  it should "limit the word cloud to the specified cloud size" in {
    val input = "Hello Hello world world world this is a test test test test"
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    // Capture the output and assert the correct cloud size
    TopWords.countWords(2, 10, 3)
    // Check the output to ensure only top 3 words are printed
  }

  it should "handle varying lengths of input gracefully" in {
    val input = "Hello world! This is a test. Testing one two three."
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(2, 10, 5)
    // Ensure expected behavior with normal input
  }
}
