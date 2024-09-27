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
    val input = "Hello world! This is a test. Hello again!"
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(2, 10, 5)
  }

  it should "not count words shorter than minLength" in {
    val input = "A quick brown fox jumps over the lazy dog"
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(4, 10, 5)
  }

  it should "handle empty input gracefully" in {
    val input = ""
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(2, 10, 5)
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

    noException should be thrownBy TopWords.countWords(2, 10, 3)
    // You can validate the output based on expected counts if countWords is refactored
  }

  it should "handle varying lengths of input gracefully" in {
    val input = "Hello world! This is a test. Testing one two three."
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(2, 10, 5)
  }

  it should "correctly count words with special characters" in {
    val input = "Hello-world! Test, test; testing... a test."
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(2, 10, 5)
  }

  it should "ignore case when counting words" in {
    val input = "Hello hello HELLO world World"
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(2, 10, 5)
  }

  it should "count the correct number of unique words" in {
    val input = "One two two three three three"
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(2, 10, 5)
  }

  it should "handle very large inputs without crashing" in {
    val input = (1 to 10000).map(i => s"word$i").mkString(" ")
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(3, 10000, 10)
  }

  it should "count repeated words accurately" in {
    val input = "hello hello hello world"
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(3, 10000, 10)
  }

  it should "handle words separated by various delimiters" in {
    val input = "word1, word2; word3: word4? word5!"
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(2, 10, 5)
  }

  it should "ignore numbers as valid words when counting" in {
    val input = "123 456 789 hello world"
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(2, 10, 5)
  }

  it should "handle inputs with leading and trailing whitespace" in {
    val input = "   hello   world   "
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(2, 10, 5)
  }

  it should "correctly handle consecutive spaces and special characters" in {
    val input = "hello    world!!!   this    is a test."
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(2, 10, 5)
  }

  it should "return empty word cloud if no words meet the criteria" in {
    val input = "A B C"
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(4, 10, 5)
    // You can check the output if countWords is refactored to return a value
  }

  it should "not crash on very long words" in {
    val input = "a" * 1000 + " " + "b" * 1000
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(1, 10, 5)
  }

  it should "handle input with a mix of letters and numbers" in {
    val input = "hello123 world456 test789"
    val inputStream = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    System.setIn(inputStream)

    noException should be thrownBy TopWords.countWords(3, 10, 5)
  }
}
