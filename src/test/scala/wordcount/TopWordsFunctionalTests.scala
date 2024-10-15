package TopWordsFunctional

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TopWordsFunctionalTests extends AnyFlatSpec with Matchers {

    "The TopWordsFunctional" should "process words without throwing an error" in {
        val words = Iterator("example", "test", "scala") // Changed to Iterator[String]
        val ignoredWords = Set("ignored") // Changed to Set[String]
        val minLength = 3
        val windowSize = 10
        val updateEvery = 5

        // Call the method; it should compile and run without errors
        noException should be thrownBy {
            TopWordsFunctional.processWords(words, ignoredWords, minLength, windowSize, updateEvery)
        }
    }
}
