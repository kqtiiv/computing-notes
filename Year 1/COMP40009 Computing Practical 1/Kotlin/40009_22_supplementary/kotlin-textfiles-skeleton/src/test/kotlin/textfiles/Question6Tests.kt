package textfiles

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlin.test.assertSame

class Question6Tests {
    private val file1: TextFile = SingleStringTextFile("File1Contents")
    private val file2: TextFile = SingleStringTextFile("File2Contents")
    private val file3: TextFile = SingleStringTextFile("File3Contents")
    private val file4: TextFile = SingleStringTextFile("File4Contents")
    private val file5: TextFile = SingleStringTextFile("File5Contents")

    @Test
    fun testGetSetAndSize() {
        val fileMap = FileMap()
        assertEquals(0, fileMap.size)
        assertNull(fileMap.get("a"))
        assertNull(fileMap.get("b"))
        assertNull(fileMap.get("c"))
        assertNull(fileMap.get("d"))
        assertNull(fileMap.get("e"))

        fileMap.set("a", file1)
        assertEquals(1, fileMap.size)
        assertSame(file1, fileMap.get("a"))
        assertNull(fileMap.get("b"))
        assertNull(fileMap.get("c"))
        assertNull(fileMap.get("d"))
        assertNull(fileMap.get("e"))

        fileMap.set("b", file2)
        assertEquals(2, fileMap.size)
        assertSame(file1, fileMap.get("a"))
        assertSame(file2, fileMap.get("b"))
        assertNull(fileMap.get("c"))
        assertNull(fileMap.get("d"))
        assertNull(fileMap.get("e"))

        fileMap.set("c", file3)
        assertEquals(3, fileMap.size)
        assertSame(file1, fileMap.get("a"))
        assertSame(file2, fileMap.get("b"))
        assertSame(file3, fileMap.get("c"))
        assertNull(fileMap.get("d"))
        assertNull(fileMap.get("e"))

        fileMap.set("d", file4)
        assertEquals(4, fileMap.size)
        assertSame(file1, fileMap.get("a"))
        assertSame(file2, fileMap.get("b"))
        assertSame(file3, fileMap.get("c"))
        assertSame(file4, fileMap.get("d"))
        assertNull(fileMap.get("e"))

        fileMap.set("e", file5)
        assertEquals(5, fileMap.size)
        assertSame(file1, fileMap.get("a"))
        assertSame(file2, fileMap.get("b"))
        assertSame(file3, fileMap.get("c"))
        assertSame(file4, fileMap.get("d"))
        assertSame(file5, fileMap.get("e"))

        fileMap.set("a", file2)
        fileMap.set("b", file3)
        fileMap.set("c", file4)
        fileMap.set("d", file5)
        fileMap.set("e", file1)

        assertSame(file2, fileMap.get("a"))
        assertSame(file3, fileMap.get("b"))
        assertSame(file4, fileMap.get("c"))
        assertSame(file5, fileMap.get("d"))
        assertSame(file1, fileMap.get("e"))
    }

    @Test
    fun testGetSetAndSizeViaOperator() {
        val fileMap = FileMap()
        assertEquals(0, fileMap.size)
        assertNull(fileMap["a"])
        assertNull(fileMap["b"])
        assertNull(fileMap["c"])
        assertNull(fileMap["d"])
        assertNull(fileMap["e"])

        fileMap["a"] = file1
        assertEquals(1, fileMap.size)
        assertSame(file1, fileMap["a"])
        assertNull(fileMap["b"])
        assertNull(fileMap["c"])
        assertNull(fileMap["d"])
        assertNull(fileMap["e"])

        fileMap["b"] = file2
        assertEquals(2, fileMap.size)
        assertSame(file1, fileMap["a"])
        assertSame(file2, fileMap["b"])
        assertNull(fileMap["c"])
        assertNull(fileMap["d"])
        assertNull(fileMap["e"])

        fileMap["c"] = file3
        assertEquals(3, fileMap.size)
        assertSame(file1, fileMap["a"])
        assertSame(file2, fileMap["b"])
        assertSame(file3, fileMap["c"])
        assertNull(fileMap["d"])
        assertNull(fileMap["e"])

        fileMap["d"] = file4
        assertEquals(4, fileMap.size)
        assertSame(file1, fileMap["a"])
        assertSame(file2, fileMap["b"])
        assertSame(file3, fileMap["c"])
        assertSame(file4, fileMap["d"])
        assertNull(fileMap["e"])

        fileMap["e"] = file5
        assertEquals(5, fileMap.size)
        assertSame(file1, fileMap["a"])
        assertSame(file2, fileMap["b"])
        assertSame(file3, fileMap["c"])
        assertSame(file4, fileMap["d"])
        assertSame(file5, fileMap["e"])

        fileMap["a"] = file2
        fileMap["b"] = file3
        fileMap["c"] = file4
        fileMap["d"] = file5
        fileMap["e"] = file1

        assertSame(file2, fileMap["a"])
        assertSame(file3, fileMap["b"])
        assertSame(file4, fileMap["c"])
        assertSame(file5, fileMap["d"])
        assertSame(file1, fileMap["e"])
    }

    @Test
    fun testIterator() {
        val fileMap = FileMap()
        val files: Array<TextFile> = Array(100) { SingleStringTextFile(it.toString()) }
        for (i in 0..<100) {
            fileMap.set("$i".padStart(2, '0'), files[i])
        }
        val iterator = fileMap.iterator()
        val retrievedContents: MutableList<Pair<String, TextFile>> = mutableListOf()
        while (iterator.hasNext()) {
            retrievedContents.add(iterator.next())
        }
        assertEquals(100, retrievedContents.size)
        val retrievedContentsSorted = retrievedContents.sortedBy { it.first }
        for (i in 0..<100) {
            assertEquals("$i".padStart(2, '0'), retrievedContentsSorted[i].first)
            assertSame(files[i], retrievedContentsSorted[i].second)
        }
    }

    @Test
    fun testIteratorViaOperator() {
        val fileMap = FileMap()
        val files: Array<TextFile> = Array(100) { SingleStringTextFile(it.toString()) }
        for (i in 0..<100) {
            fileMap.set("$i".padStart(2, '0'), files[i])
        }
        val retrievedContents: MutableList<Pair<String, TextFile>> = mutableListOf()
        for (entry in fileMap) {
            retrievedContents.add(entry)
        }
        assertEquals(100, retrievedContents.size)
        val retrievedContentsSorted = retrievedContents.sortedBy { it.first }
        for (i in 0..<100) {
            assertEquals("$i".padStart(2, '0'), retrievedContentsSorted[i].first)
            assertSame(files[i], retrievedContentsSorted[i].second)
        }
    }

    @Test
    fun performanceTest() {
        val numElements = 1000000
        val fileMap: FileMap = FileMap()
        val files: Array<TextFile> = Array(numElements) { SingleStringTextFile(it.toString()) }
        (0..<numElements).forEach {
            assertEquals(it, fileMap.size)
            fileMap.set("f$it", files[it])
        }
        assertEquals(numElements, fileMap.size)
        for (i in 0..<numElements) {
            assertSame(files[i], fileMap.get("f$i"))
        }
    }
}
