package textfiles

import java.util.Random
import kotlin.test.Test
import kotlin.test.assertEquals

class Question4Tests {
    @Test
    fun concurrencyTest() {
        for (repeat in 1..20) {
            println("Repeat run $repeat")
            val initialText = "initialtext"
            val authorStrings: List<List<String>> =
                (0..<8).map {
                    (0..<1000).map {
                        (0..it % 10).map { number -> number.toString() }.joinToString(separator = "")
                    }
                }
            val expectedOutput: String =
                (initialText + authorStrings.flatten().joinToString(separator = ""))
                    .toCharArray()
                    .sortedArray()
                    .joinToString(separator = "")

            val singleStringTextFile = SingleStringTextFile(initialText)

            val threadSafeTextFile = ThreadSafeTextFile(singleStringTextFile)

            val threadList: List<Thread> =
                List(8) {
                    Thread(Author(authorStrings[it], threadSafeTextFile, Random()))
                }

            threadList.forEach { it.start() }
            threadList.forEach { it.join() }

            assertEquals(
                expectedOutput,
                threadSafeTextFile
                    .toString()
                    .toCharArray()
                    .sortedArray()
                    .joinToString(separator = ""),
            )
        }
    }
}
