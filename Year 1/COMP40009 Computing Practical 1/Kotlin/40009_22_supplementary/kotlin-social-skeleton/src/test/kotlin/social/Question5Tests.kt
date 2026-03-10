package social

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class Question5Tests {
    @Test
    fun `test example in appendix`() {
        val map = HashMapLinked<String, Int>()
        assertEquals(emptyList(), map.values)
        map.set("a", 14)
        assertEquals(listOf(14), map.values)
        map.set("b", 61)
        assertEquals(listOf(14, 61), map.values)
        map.set("f", 11)
        assertEquals(listOf(14, 61, 11), map.values)
        map.set("j", 18)
        assertEquals(listOf(14, 61, 11, 18), map.values)
        map.set("b", 2)
        assertEquals(listOf(14, 11, 18, 2), map.values)
        map.remove("a")
        assertEquals(listOf(11, 18, 2), map.values)
    }

    @Test
    fun `test set size remove`() {
        val map = HashMapLinked<String, Int>()
        assertEquals(0, map.size)
        assertNull(map.set("a", 2))
        assertEquals(1, map.size)
        assertEquals(2, map.set("a", 3))
        assertEquals(1, map.size)
        assertEquals(3, map.remove("a"))
        assertEquals(0, map.size)
        assertNull(map.remove("a"))
        assertEquals(0, map.size)
    }

    @Test
    fun `test values 1`() {
        val map = HashMapLinked<String, Int>()
        map["a"] = 1
        assertEquals(listOf(1), map.values)
        map["z"] = 2
        assertEquals(listOf(1, 2), map.values)
        map["w"] = 3
        assertEquals(listOf(1, 2, 3), map.values)
        map["x"] = 3
        assertEquals(listOf(1, 2, 3, 3), map.values)
        map["p"] = 5
        assertEquals(listOf(1, 2, 3, 3, 5), map.values)
        map["a"] = 42
        assertEquals(listOf(2, 3, 3, 5, 42), map.values)
        map["w"] = 16
        assertEquals(listOf(2, 3, 5, 42, 16), map.values)
        map["t"] = 23
        assertEquals(listOf(2, 3, 5, 42, 16, 23), map.values)
        map["z"] = 105
        assertEquals(listOf(3, 5, 42, 16, 23, 105), map.values)
    }

    @Test
    fun `test values 2`() {
        val map = HashMapLinked<String, Int>()
        map["a"] = 1
        assertEquals(listOf(1), map.values)
        map["z"] = 2
        assertEquals(listOf(1, 2), map.values)
        map["w"] = 3
        assertEquals(listOf(1, 2, 3), map.values)
        map["x"] = 3
        assertEquals(listOf(1, 2, 3, 3), map.values)
        map["p"] = 5
        assertEquals(listOf(1, 2, 3, 3, 5), map.values)
        map.remove("w")
        assertEquals(listOf(1, 2, 3, 5), map.values)
        map.remove("a")
        assertEquals(listOf(2, 3, 5), map.values)
        map.remove("p")
        assertEquals(listOf(2, 3), map.values)
        map.remove("z")
        assertEquals(listOf(3), map.values)
        map.remove("x")
        assertEquals(emptyList(), map.values)
    }

    @Test
    fun `test remove oldest entry 1`() {
        val map = HashMapLinked<String, Int>()
        assertNull(map.removeLongestStandingEntry())
    }

    @Test
    fun `test remove oldest entry 2`() {
        val map = HashMapLinked<String, Int>()
        map["hello"] = 42
        assertEquals(1, map.size)
        assertTrue(map.containsKey("hello"))
        assertEquals("hello" to 42, map.removeLongestStandingEntry())
        assertFalse(map.containsKey("hello"))
        assertEquals(0, map.size)
    }

    @Test
    fun `test remove oldest entry 3`() {
        val map = HashMapLinked<String, Int>()
        map["hello"] = 42
        map["hell"] = 41
        map["hel"] = 40
        map["he"] = 39
        map["h"] = 38

        assertTrue(map.containsKey("hello"))
        assertTrue(map.containsKey("hell"))
        assertTrue(map.containsKey("hel"))
        assertTrue(map.containsKey("he"))
        assertTrue(map.containsKey("h"))
        assertEquals(5, map.size)

        assertEquals("hello" to 42, map.removeLongestStandingEntry())
        assertFalse(map.containsKey("hello"))
        assertTrue(map.containsKey("hell"))
        assertTrue(map.containsKey("hel"))
        assertTrue(map.containsKey("he"))
        assertTrue(map.containsKey("h"))
        assertEquals(4, map.size)

        assertEquals("hell" to 41, map.removeLongestStandingEntry())
        assertFalse(map.containsKey("hello"))
        assertFalse(map.containsKey("hell"))
        assertTrue(map.containsKey("hel"))
        assertTrue(map.containsKey("he"))
        assertTrue(map.containsKey("h"))
        assertEquals(3, map.size)

        assertEquals("hel" to 40, map.removeLongestStandingEntry())
        assertFalse(map.containsKey("hello"))
        assertFalse(map.containsKey("hell"))
        assertFalse(map.containsKey("hel"))
        assertTrue(map.containsKey("he"))
        assertTrue(map.containsKey("h"))
        assertEquals(2, map.size)

        assertEquals("he" to 39, map.removeLongestStandingEntry())
        assertFalse(map.containsKey("hello"))
        assertFalse(map.containsKey("hell"))
        assertFalse(map.containsKey("hel"))
        assertFalse(map.containsKey("he"))
        assertTrue(map.containsKey("h"))
        assertEquals(1, map.size)

        assertEquals("h" to 38, map.removeLongestStandingEntry())
        assertFalse(map.containsKey("hello"))
        assertFalse(map.containsKey("hell"))
        assertFalse(map.containsKey("hel"))
        assertFalse(map.containsKey("he"))
        assertFalse(map.containsKey("h"))
        assertEquals(0, map.size)

        assertNull(map.removeLongestStandingEntry())
    }

    @Test
    fun `performance test`() {
        val numElements = 1000000
        val map = HashMapLinked<String, Int>()
        (0..<numElements).forEach {
            assertEquals(it, map.size)
            map.set("v$it", it)
        }
        assertEquals(numElements, map.size)
        for (i in 0..<numElements) {
            assertEquals(i, map.remove("v$i"))
        }
    }

    @Test
    fun `performance test 2`() {
        val numElements = 1000000
        val map = HashMapLinked<String, Int>()
        (0..<numElements).forEach {
            assertEquals(it, map.size)
            map.set("v$it", it)
        }
        assertEquals(numElements, map.size)
        assertEquals((0..<numElements).toList(), map.values)
        for (i in 0..<numElements step 2) {
            assertEquals(i, map.remove("v$i"))
        }
        assertEquals(numElements / 2, map.size)
        assertEquals((1..<numElements step 2).toList(), map.values)
    }
}
