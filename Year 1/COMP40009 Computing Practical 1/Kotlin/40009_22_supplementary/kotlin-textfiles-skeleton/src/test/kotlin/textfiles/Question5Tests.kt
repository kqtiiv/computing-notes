package textfiles

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class Question5Tests {
    @Test
    fun testCompareTo1() {
        val t1: TextFile = SingleStringTextFile("Hello")
        val t2: TextFile = SingleStringTextFile("Hello")
        assertEquals(0, t1.compareTo(t2))
        assertEquals(0, t2.compareTo(t1))
    }

    @Test
    fun testCompareTo2() {
        val t1: TextFile = SingleStringTextFile("Hell")
        val t2: TextFile = SingleStringTextFile("Hello")
        assertTrue(t1 < t2)
        assertTrue(t2 > t1)
    }

    @Test
    fun testCompareTo3() {
        val t1: TextFile = MultiStringTextFile("Hello")
        val t2: TextFile = SingleStringTextFile("Hello")
        assertEquals(0, t1.compareTo(t2))
        assertEquals(0, t2.compareTo(t1))
    }

    @Test
    fun testCompareTo4() {
        val t1: TextFile = MultiStringTextFile("Aello")
        val t2: TextFile = SingleStringTextFile("Hello")
        assertTrue(t1 < t2)
        assertTrue(t2 > t1)
    }

    @Test
    fun testCompareTo5() {
        val t1: TextFile = MultiStringTextFile("Hello")
        val t2: TextFile = MultiStringTextFile("Hello")
        assertEquals(0, t1.compareTo(t2))
        assertEquals(0, t2.compareTo(t1))
    }

    @Test
    fun testCompareTo6() {
        val t1: TextFile = MultiStringTextFile("Hella")
        val t2: TextFile = MultiStringTextFile("Hello")
        assertTrue(t1 < t2)
        assertTrue(t2 > t1)
    }
}
