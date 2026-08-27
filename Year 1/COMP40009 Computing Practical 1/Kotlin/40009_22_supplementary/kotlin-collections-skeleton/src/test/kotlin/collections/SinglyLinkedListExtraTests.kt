package collections

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.fail

// These are examples of the kinds of imports that may be useful when writing tests.
import kotlin.test.assertFalse
import kotlin.test.assertTrue

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

class SinglyLinkedListExtraTests {
    @Test
    fun `test removeAt in bounds from end (Int)`() {
        val list = SinglyLinkedList<Int>()
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
        val list = SinglyLinkedList<Int>()
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
        val list = SinglyLinkedList<Int>()
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
        val list = SinglyLinkedList<String>()
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
        val list = SinglyLinkedList<String>()
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
        val list = SinglyLinkedList<Int>()
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
        val list = SinglyLinkedList<Int>()
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
        val list = SinglyLinkedList<Int>()
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
        val list = SinglyLinkedList<Int>()

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
        val list = SinglyLinkedList<String>()
        for (i in 1..10) {
            list.add(i - 1, exampleStrings[i - 1])
        }
        assertTrue(list.remove(exampleStrings[5]))
        assertEquals(list.size, 9)
        assertTrue(list.remove(exampleStrings[6]))
        assertEquals(list.size, 8)
    }
}
