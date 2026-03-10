package collections

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue
import kotlin.test.fail

private val exampleStrings: Array<String> =
    arrayOf(
        "cat",
        "dog",
        "frog",
        "horse",
        "zebra",
        "wildebeest",
        "vulture",
        "hyena",
        "warthog",
        "hyrax",
    )

class SinglyLinkedListJavaTests {
    @Test
    fun `test get (String)`() {
        val list = SinglyLinkedListJava<String>()
        for (i in 0..<10) {
            list.add(i, exampleStrings[i])
            assertEquals(i + 1, list.size)
        }
        for (i in 0..<10) {
            assertEquals(exampleStrings[i], list[i])
        }
    }

    @Test
    fun `test add (String)`() {
        val list = SinglyLinkedListJava<String>()
        for (i in 1..10) {
            list.add(i - 1, exampleStrings[i - 1])
            assertEquals(i, list.size)
        }
        assertEquals(
            exampleStrings.joinToString(prefix = "[", postfix = "]"),
            list.toString(),
        )
    }

    @Test
    fun `test add in middle (String)`() {
        val list = SinglyLinkedListJava<String>()
        for (i in 1..5) {
            list.add(i - 1, exampleStrings[i])
            assertEquals(i, list.size)
        }
        assertEquals(5, list.size)
        list.add(3, "blob")
        assertEquals(6, list.size)
        assertEquals("[dog, frog, horse, blob, zebra, wildebeest]", list.toString())
    }

    @Test
    fun `test add at end (String)`() {
        val list = SinglyLinkedListJava<String>()
        for (i in 1..5) {
            list.add(i - 1, exampleStrings[i])
            assertEquals(i, list.size)
        }
        list.add(5, "blob")
        assertEquals(6, list.size)
        assertEquals("[dog, frog, horse, zebra, wildebeest, blob]", list.toString())
    }

    @Test
    fun `test clear (String)`() {
        val list = SinglyLinkedListJava<String>()
        for (i in 1..5) {
            list.add(i - 1, exampleStrings[i])
            assertEquals(i, list.size)
        }
        list.clear()
        assertEquals(0, list.size)
        assertEquals("[]", list.toString())
        list.clear()
        assertEquals(0, list.size)
        assertEquals("[]", list.toString())
    }

    @Test
    fun `test toString empty (String)`() {
        val list = SinglyLinkedListJava<String>()
        assertEquals(0, list.size)
        assertEquals("[]", list.toString())
    }

    @Test
    fun `test set (String)`() {
        val list = SinglyLinkedListJava<String>()
        list.add(0, "one")
        list.add(1, "two")
        list.add(2, "three")
        assertEquals("one", list.set(0, "forty two"))
        assertEquals("two", list.set(1, "forty three"))
        assertEquals("three", list.set(2, "forty four"))
        assertEquals(3, list.size)
        assertEquals("[forty two, forty three, forty four]", list.toString())
    }

    @Test
    fun `test get (Int)`() {
        val list = SinglyLinkedListJava<Int>()
        for (i in 1..10) {
            list.add(i - 1, i)
            assertEquals(i, list.size)
        }
        for (i in 1..10) {
            assertEquals(i, list[i - 1])
        }
    }

    @Test
    fun `test add (Int)`() {
        val list = SinglyLinkedListJava<Int>()
        for (i in 1..10) {
            list.add(i - 1, i)
            assertEquals(i, list.size)
        }
        assertEquals("[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]", list.toString())
    }

    @Test
    fun `test add in middle (Int)`() {
        val list = SinglyLinkedListJava<Int>()
        for (i in 1..5) {
            list.add(i - 1, i)
            assertEquals(i, list.size)
        }
        assertEquals(5, list.size)
        list.add(3, 42)
        assertEquals(6, list.size)
        assertEquals("[1, 2, 3, 42, 4, 5]", list.toString())
    }

    @Test
    fun `test add at end (Int)`() {
        val list = SinglyLinkedListJava<Int>()
        for (i in 1..5) {
            list.add(i - 1, i)
            assertEquals(i, list.size)
        }
        list.add(5, 42)
        assertEquals(6, list.size)
        assertEquals("[1, 2, 3, 4, 5, 42]", list.toString())
    }

    @Test
    fun `test clear (Int)`() {
        val list = SinglyLinkedListJava<Int>()
        for (i in 1..5) {
            list.add(i - 1, i)
            assertEquals(i, list.size)
        }
        list.clear()
        assertEquals(0, list.size)
        assertEquals("[]", list.toString())
        list.clear()
        assertEquals(0, list.size)
        assertEquals("[]", list.toString())
    }

    @Test
    fun `test toString empty (Int)`() {
        val list = SinglyLinkedListJava<Int>()
        assertEquals(0, list.size)
        assertEquals("[]", list.toString())
        val trivialList = SinglyLinkedListJava<Int>()
        assertEquals(0, trivialList.size)
        assertEquals("[]", trivialList.toString())
    }

    @Test
    fun `test set (Int)`() {
        val list = SinglyLinkedListJava<Int>()
        list.add(0, 1)
        list.add(1, 2)
        list.add(2, 3)
        assertEquals(1, list.set(0, 42))
        assertEquals(2, list.set(1, 43))
        assertEquals(3, list.set(2, 44))
        assertEquals(3, list.size)
        assertEquals("[42, 43, 44]", list.toString())
    }

    @Test
    fun `test addAll small`() {
        val firstSmallList: ImperialMutableList<Int> = SinglyLinkedListJava()
        val secondSmallList: ImperialMutableList<Int> = SinglyLinkedListJava()
        for (i in 1..10) {
            firstSmallList.add(i - 1, i)
            secondSmallList.add(i - 1, -i)
        }
        firstSmallList.addAll(firstSmallList.size, secondSmallList)
        assertEquals(20, firstSmallList.size)
        assertEquals(1, firstSmallList[0])
        assertEquals(2, firstSmallList[1])
        assertEquals(3, firstSmallList[2])
        assertEquals(4, firstSmallList[3])
        assertEquals(5, firstSmallList[4])
        assertEquals(6, firstSmallList[5])
        assertEquals(7, firstSmallList[6])
        assertEquals(8, firstSmallList[7])
        assertEquals(9, firstSmallList[8])
        assertEquals(10, firstSmallList[9])
        assertEquals(-1, firstSmallList[10])
        assertEquals(-2, firstSmallList[11])
        assertEquals(-3, firstSmallList[12])
        assertEquals(-4, firstSmallList[13])
        assertEquals(-5, firstSmallList[14])
        assertEquals(-6, firstSmallList[15])
        assertEquals(-7, firstSmallList[16])
        assertEquals(-8, firstSmallList[17])
        assertEquals(-9, firstSmallList[18])
        assertEquals(-10, firstSmallList[19])
    }

    @Test
    fun `test addAll at index small`() {
        val firstSmallList: ImperialMutableList<Int> = SinglyLinkedListJava()
        val secondSmallList: ImperialMutableList<Int> = SinglyLinkedListJava()
        for (i in 1..10) {
            firstSmallList.add(i - 1, i)
            secondSmallList.add(i - 1, -i)
        }
        firstSmallList.addAll(5, secondSmallList)
        assertEquals(20, firstSmallList.size)
        assertEquals(1, firstSmallList[0])
        assertEquals(2, firstSmallList[1])
        assertEquals(3, firstSmallList[2])
        assertEquals(4, firstSmallList[3])
        assertEquals(5, firstSmallList[4])
        assertEquals(-1, firstSmallList[5])
        assertEquals(-2, firstSmallList[6])
        assertEquals(-3, firstSmallList[7])
        assertEquals(-4, firstSmallList[8])
        assertEquals(-5, firstSmallList[9])
        assertEquals(-6, firstSmallList[10])
        assertEquals(-7, firstSmallList[11])
        assertEquals(-8, firstSmallList[12])
        assertEquals(-9, firstSmallList[13])
        assertEquals(-10, firstSmallList[14])
        assertEquals(6, firstSmallList[15])
        assertEquals(7, firstSmallList[16])
        assertEquals(8, firstSmallList[17])
        assertEquals(9, firstSmallList[18])
        assertEquals(10, firstSmallList[19])
    }

    @Test
    fun `test addAll at start`() {
        val list1: ImperialMutableList<Int> = SinglyLinkedListJava()
        val list2: ImperialMutableList<Int> = SinglyLinkedListJava()
        list1.add(0, 1)
        list1.add(1, 2)
        list1.add(2, 3)
        list2.add(0, 4)
        list2.add(1, 5)
        list2.add(2, 6)
        list1.addAll(0, list2)
        assertEquals(6, list1.size)
        assertEquals(4, list1[0])
        assertEquals(5, list1[1])
        assertEquals(6, list1[2])
        assertEquals(1, list1[3])
        assertEquals(2, list1[4])
        assertEquals(3, list1[5])
    }

    @Test
    fun `test addAll at end`() {
        val list1: ImperialMutableList<Int> = SinglyLinkedListJava()
        val list2: ImperialMutableList<Int> = SinglyLinkedListJava()
        list1.add(0, 1)
        list1.add(1, 2)
        list1.add(2, 3)
        list2.add(0, 4)
        list2.add(1, 5)
        list2.add(2, 6)
        list1.addAll(3, list2)
        assertEquals(6, list1.size)
        assertEquals(1, list1[0])
        assertEquals(2, list1[1])
        assertEquals(3, list1[2])
        assertEquals(4, list1[3])
        assertEquals(5, list1[4])
        assertEquals(6, list1[5])
    }

    @Test
    fun `size is 0`() {
        assertEquals(0, SinglyLinkedListJava<Int>().size)
    }

    @Test
    fun `iterator has next when empty`() {
        assertFalse(SinglyLinkedListJava<Int>().iterator().hasNext())
    }

    @Test
    fun `iterate over elements`() {
        val list: ImperialMutableList<String> = SinglyLinkedListJava()
        list.add(0, "a")
        list.add(1, "b")
        list.add(2, "c")
        val iterator = list.iterator()
        assertTrue(iterator.hasNext())
        assertEquals("a", iterator.next())
        assertTrue(iterator.hasNext())
        assertEquals("b", iterator.next())
        assertTrue(iterator.hasNext())
        assertEquals("c", iterator.next())
        assertFalse(iterator.hasNext())
    }

    @Test
    fun `concatenate with iterator`() {
        val list: ImperialMutableList<String> = SinglyLinkedListJava()
        list.add(0, "a")
        list.add(1, "b")
        list.add(2, "c")
        val concatenation = StringBuilder()
        for (string in list) {
            concatenation.append(string)
        }
        assertEquals("abc", concatenation.toString())
    }

    @Test
    fun `test removeAt in bounds from end (Int)`() {
        val list = SinglyLinkedListJava<Int>()
        for (i in 1..10) {
            list.add(i - 1, i)
        }
        for (i in 10 downTo 1) {
            val removed = list.removeAt(i - 1)
            assertEquals(removed, i)
            assertEquals(list.size, i - 1)
        }
    }

    @Test
    fun `test removeAt out of bounds on empty list (Int)`() {
        val list = SinglyLinkedListJava<Int>()
        try {
            list.removeAt(0)
            fail()
        } catch (_: IndexOutOfBoundsException) {
            // good
        }
        assertEquals(list.size, 0)
    }

    @Test
    fun `test removeAt out of bounds on non-empty list (Int)`() {
        val list = SinglyLinkedListJava<Int>()
        for (i in 1..10) {
            list.add(i - 1, i)
        }

        try {
            list.removeAt(10)
            fail()
        } catch (_: IndexOutOfBoundsException) {
            // good caught upper bound
        }

        assertEquals(list.size, 10)

        try {
            list.removeAt(-1)
            fail()
        } catch (_: IndexOutOfBoundsException) {
            // good: caught lower bound
        }

        assertEquals(list.size, 10)
    }

    @Test
    fun `test removeAt in bounds from end (String)`() {
        val list = SinglyLinkedListJava<String>()
        for (i in 1..10) {
            list.add(i - 1, exampleStrings[i - 1])
        }
        for (i in 10 downTo 1) {
            val removed = list.removeAt(i - 1)
            assertEquals(removed, exampleStrings[i - 1])
            assertEquals(list.size, i - 1)
        }
    }

    @Test
    fun `test removeAt in bounds from middle (String)`() {
        val list = SinglyLinkedListJava<String>()
        for (i in 1..10) {
            list.add(i - 1, exampleStrings[i - 1])
        }
        assertEquals(list.removeAt(5), exampleStrings[5])
        assertEquals(list.size, 9)
        assertEquals(list.removeAt(5), exampleStrings[6])
        assertEquals(list.size, 8)
    }

    @Test
    fun `test remove with (Int) that does appear in list`() {
        val list = SinglyLinkedListJava<Int>()
        for (i in 1..10) {
            list.add(i - 1, i)
        }
        for (i in 10 downTo 1) {
            val removed = list.remove(i)
            assertTrue(removed)
            assertEquals(list.size, i - 1)
        }
    }

    @Test
    fun `test remove on empty list (Int)`() {
        val list = SinglyLinkedListJava<Int>()
        try {
            list.remove(0)
            fail()
        } catch (_: AssertionError) {
            // good
        }
        assertEquals(list.size, 0)
    }

    @Test
    fun `test remove on (Int) that do not appear in list`() {
        val list = SinglyLinkedListJava<Int>()
        var removed = true

        for (i in 1..10) {
            list.add(i - 1, i)
        }

        try {
            removed = list.remove(11)
            fail()
        } catch (_: AssertionError) {
            // good caught upper bound
        }

        assertEquals(list.size, 10)
        assertFalse(removed)
        removed = true

        try {
            removed = list.remove(-1)
            fail()
        } catch (_: AssertionError) {
            // good: caught lower bound
        }

        assertEquals(list.size, 10)
        assertFalse(removed)
    }

    @Test
    fun `test remove (Int) removes first item when there are duplicates in list`() {
        val list = SinglyLinkedListJava<Int>()

        list.add(0, 10)
        list.add(1, 9)
        list.add(2, 8)
        list.add(3, 9)

        val removed = list.remove(9)
        assertTrue(removed)
        assertEquals(list.size, 3)
        assertEquals(list[2], 9)
    }

    @Test
    fun `test remove on list with (String)`() {
        val list = SinglyLinkedListJava<String>()
        for (i in 1..10) {
            list.add(i - 1, exampleStrings[i - 1])
        }
        assertTrue(list.remove(exampleStrings[5]))
        assertEquals(list.size, 9)
        assertTrue(list.remove(exampleStrings[6]))
        assertEquals(list.size, 8)
    }
}
