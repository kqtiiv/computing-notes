package textfiles

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.fail

class Question3Tests {
    @Test
    fun testLength1() {
        val textFile: TextFile = LazyTextFile(SingleStringTextFile(""))
        assertEquals(0, textFile.length)
    }

    @Test
    fun testLength2() {
        val textFile: TextFile = LazyTextFile(SingleStringTextFile("Hello\nWorld"))
        assertEquals(11, textFile.length)
    }

    @Test
    fun testLength3() {
        val textFile: TextFile =
            LazyTextFile(
                SingleStringTextFile(
                    "The quick brown fox jumped over the lazy dog ten times today",
                ),
            )
        assertEquals(60, textFile.length)
    }

    @Test
    fun testInsert1() {
        val textFile: TextFile = LazyTextFile(SingleStringTextFile(""))
        textFile.insertText(0, "First")
        textFile.insertText(0, "Second")
        textFile.insertText(0, "Third")
        assertEquals("ThirdSecondFirst", textFile.toString())
    }

    @Test
    fun testInsert2() {
        val textFile: TextFile = LazyTextFile(SingleStringTextFile("SomeContent"))
        textFile.insertText(11, "MoreContent")
        textFile.insertText(22, "EvenMoreContent")
        assertEquals("SomeContentMoreContentEvenMoreContent", textFile.toString())
    }

    @Test
    fun testInsert3() {
        val textFile: TextFile = LazyTextFile(SingleStringTextFile("SomeContent"))
        textFile.insertText(0, "!")
        textFile.insertText(2, "!")
        textFile.insertText(4, "!")
        textFile.insertText(6, "!")
        textFile.insertText(8, "!")
        textFile.insertText(10, "!")
        textFile.insertText(12, "!")
        textFile.insertText(14, "!")
        textFile.insertText(16, "!")
        textFile.insertText(18, "!")
        textFile.insertText(20, "!")
        textFile.insertText(22, "!")
        assertEquals("!S!o!m!e!C!o!n!t!e!n!t!", textFile.toString())
    }

    @Test
    fun testInsertException1() {
        try {
            val textFile: TextFile = LazyTextFile(SingleStringTextFile("SomeContent"))
            textFile.insertText(-1, "DoNotAddMe")
            textFile.insertText(0, "a")
        } catch (exception: FileIndexOutOfBoundsException) {
            // Good: an exception should have been thrown.
            return
        }
        fail()
    }

    @Test
    fun testInsertException2() {
        try {
            val textFile: TextFile = LazyTextFile(SingleStringTextFile("SomeContent"))
            textFile.insertText(12, "DoNotAddMe")
            textFile.insertText(0, "a")
        } catch (exception: FileIndexOutOfBoundsException) {
            // Good: an exception should have been thrown.
            return
        }
        fail()
    }

    @Test
    fun testInsertException3() {
        try {
            val textFile: TextFile = LazyTextFile(SingleStringTextFile("SomeContent"))
            textFile.insertText(12, "")
            textFile.insertText(0, "a")
        } catch (exception: FileIndexOutOfBoundsException) {
            // Good: an exception should have been thrown.
            return
        }
        fail()
    }

    @Test
    fun testDelete1() {
        val textFile: TextFile = LazyTextFile(SingleStringTextFile(""))
        textFile.deleteText(0, 0)
        assertEquals("", textFile.toString())
    }

    @Test
    fun testDelete2() {
        val textFile: TextFile = LazyTextFile(SingleStringTextFile("Hello"))
        textFile.deleteText(3, 0)
        assertEquals("Hello", textFile.toString())
    }

    @Test
    fun testDelete3() {
        val textFile: TextFile = LazyTextFile(SingleStringTextFile("Example text file"))
        textFile.deleteText(0, 17)
        assertEquals("", textFile.toString())
    }

    @Test
    fun testDelete4() {
        val textFile: TextFile = LazyTextFile(SingleStringTextFile("Example text file"))
        textFile.deleteText(8, 5)
        assertEquals("Example file", textFile.toString())
    }

    @Test
    fun testDelete5() {
        val textFile: TextFile = LazyTextFile(SingleStringTextFile("T"))
        textFile.deleteText(1, 0)
        assertEquals("T", textFile.toString())
    }

    @Test
    fun testDelete6() {
        val textFile: TextFile =
            LazyTextFile(
                SingleStringTextFile(
                    "The quick brown fox jumped over the lazy dog ten times today",
                ),
            )
        textFile.deleteText(3, 43)
        assertEquals("Theen times today", textFile.toString())
    }

    @Test
    fun testDeleteException1() {
        try {
            val textFile: TextFile = LazyTextFile(SingleStringTextFile("SomeContent"))
            textFile.deleteText(-1, 3)
        } catch (exception: FileIndexOutOfBoundsException) {
            // Good: an exception should have been thrown.
            return
        }
        fail()
    }

    @Test
    fun testDeleteException2() {
        try {
            val textFile: TextFile = LazyTextFile(SingleStringTextFile("SomeContent"))
            textFile.deleteText(0, 12)
        } catch (exception: FileIndexOutOfBoundsException) {
            // Good: an exception should have been thrown.
            return
        }
        fail()
    }

    @Test
    fun testDeleteException3() {
        try {
            val textFile: TextFile = LazyTextFile(SingleStringTextFile("SomeContent"))
            textFile.deleteText(10, 2)
        } catch (exception: FileIndexOutOfBoundsException) {
            // Good: an exception should have been thrown.
            return
        }
        fail()
    }

    @Test
    fun testDeleteException4() {
        try {
            val textFile: TextFile = LazyTextFile(SingleStringTextFile("SomeContent"))
            textFile.deleteText(100, 0)
        } catch (exception: FileIndexOutOfBoundsException) {
            // Good: an exception should have been thrown.
            return
        }
        fail()
    }

    @Test
    fun testDeleteException5() {
        try {
            val textFile: TextFile = LazyTextFile(SingleStringTextFile("SomeContent"))
            textFile.deleteText(0, -1)
        } catch (exception: FileIndexOutOfBoundsException) {
            // Good: an exception should have been thrown.
            return
        }
        fail()
    }

    @Test
    fun testLaziness() {
        val operationCountingTextFile = OperationCountingTextFile()
        val lazyTextFile: TextFile = LazyTextFile(operationCountingTextFile)
        assertEquals(0, operationCountingTextFile.numInsertions)
        assertEquals(0, operationCountingTextFile.numDeletions)

        lazyTextFile.insertText(0, "hello")
        assertEquals(0, operationCountingTextFile.numInsertions)
        assertEquals(0, operationCountingTextFile.numDeletions)

        lazyTextFile.insertText(0, "hello")
        assertEquals(0, operationCountingTextFile.numInsertions)
        assertEquals(0, operationCountingTextFile.numDeletions)

        lazyTextFile.insertText(0, "hello")
        assertEquals(0, operationCountingTextFile.numInsertions)
        assertEquals(0, operationCountingTextFile.numDeletions)

        lazyTextFile.length
        assertEquals(1, operationCountingTextFile.numInsertions)
        assertEquals(0, operationCountingTextFile.numDeletions)

        lazyTextFile.insertText(0, "hello")
        assertEquals(1, operationCountingTextFile.numInsertions)
        assertEquals(0, operationCountingTextFile.numDeletions)

        lazyTextFile.deleteText(5, 2)
        assertEquals(2, operationCountingTextFile.numInsertions)
        assertEquals(1, operationCountingTextFile.numDeletions)

        lazyTextFile.insertText(0, "hello")
        assertEquals(2, operationCountingTextFile.numInsertions)
        assertEquals(1, operationCountingTextFile.numDeletions)

        lazyTextFile.insertText(0, "hello")
        assertEquals(2, operationCountingTextFile.numInsertions)
        assertEquals(1, operationCountingTextFile.numDeletions)

        //noinspection ResultOfMethodCallIgnored
        lazyTextFile.toString()
        assertEquals(3, operationCountingTextFile.numInsertions)
        assertEquals(1, operationCountingTextFile.numDeletions)

        lazyTextFile.insertText(0, "hello")
        assertEquals(3, operationCountingTextFile.numInsertions)
        assertEquals(1, operationCountingTextFile.numDeletions)

        lazyTextFile.insertText(1, "hello")
        assertEquals(4, operationCountingTextFile.numInsertions)
        assertEquals(1, operationCountingTextFile.numDeletions)

        lazyTextFile.insertText(1, "hello")
        assertEquals(4, operationCountingTextFile.numInsertions)
        assertEquals(1, operationCountingTextFile.numDeletions)

        lazyTextFile.deleteText(1, 6)
        assertEquals(5, operationCountingTextFile.numInsertions)
        assertEquals(2, operationCountingTextFile.numDeletions)
    }

    class OperationCountingTextFile : TextFile {
        var numInsertions = 0
            private set

        var numDeletions = 0
            private set

        override val length: Int
            get() = 1000

        override fun insertText(
            offset: Int,
            toInsert: String,
        ) {
            numInsertions++
        }

        override fun deleteText(
            offset: Int,
            size: Int,
        ) {
            numDeletions++
        }
    }
}
