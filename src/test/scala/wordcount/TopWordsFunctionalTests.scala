package TopWordsFunctional

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TopWordsFunctionalTests extends AnyFlatSpec with Matchers {

    "The TopWordsFunctional" should "process words without throwing an error" in {
        val words = Iterator("example", "test", "scala")
        val ignoredWords = Set("ignored")
        val minLength = 3
        val windowSize = 10
        val updateEvery = 5

        // Call the method; it should compile and run without errors
        noException should be thrownBy {
            TopWordsFunctional.processWords(words, ignoredWords, minLength, windowSize, updateEvery)
        }
    }

    it should "ignore specified ignored words" in {
        val words = Iterator("example", "ignored", "scala", "test")
        val ignoredWords = Set("ignored")
        val minLength = 3
        val windowSize = 10
        val updateEvery = 1

        val results = TopWordsFunctional.processWords(words, ignoredWords, minLength, windowSize, updateEvery).toList
        results.foreach { case (counts, _) =>
            counts should not contain key("ignored")
        }
    }

    it should "filter out words shorter than minimum length" in {
        val words = Iterator("hi", "a", "ok", "scala", "test")
        // Explicitly specify the type of ignoredWords
        val ignoredWords: Set[String] = Set() 
        val minLength = 3
        val windowSize = 10
        val updateEvery = 1

        val results = TopWordsFunctional.processWords(words, ignoredWords, minLength, windowSize, updateEvery).toList
        results.foreach { case (counts, _) =>
            counts.keys.foreach { word =>
                word.length should be >= minLength
            }
        }
    }

    it should "count word occurrences accurately" in {
        val words = Iterator("scala", "scala", "test", "test", "test", "example")
        val ignoredWords: Set[String] = Set() 
        val minLength = 3
        val windowSize = 10
        val updateEvery = 1

        val results = TopWordsFunctional.processWords(words, ignoredWords, minLength, windowSize, updateEvery).toList
        results.foreach { case (counts, _) =>
            counts("test") should be(3)
            counts("scala") should be(2)
            counts("example") should be(1)
        }
    }

    it should "handle empty input correctly" in {
        val words = Iterator.empty
        val ignoredWords: Set[String] = Set() 
        val minLength = 3
        val windowSize = 10
        val updateEvery = 1

        val results = TopWordsFunctional.processWords(words, ignoredWords, minLength, windowSize, updateEvery).toList
        results should be (empty) 
    }

    it should "handle input with only ignored words" in {
        val words = Iterator("ignored", "ignored", "ignored")
        val ignoredWords = Set("ignored")
        val minLength = 3
        val windowSize = 10
        val updateEvery = 1

        val results = TopWordsFunctional.processWords(words, ignoredWords, minLength, windowSize, updateEvery).toList
        results should be (empty)
    }

}
