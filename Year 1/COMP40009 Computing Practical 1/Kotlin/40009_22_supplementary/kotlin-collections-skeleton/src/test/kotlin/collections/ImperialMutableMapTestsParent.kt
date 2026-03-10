package collections

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotNull
import kotlin.test.assertNull
import kotlin.test.assertTrue
import kotlin.test.fail

abstract class ImperialMutableMapTestsParent {
    abstract fun emptyCustomMutableMapStringInt(): ImperialMutableMap<String, Int>

    abstract fun emptyCustomMutableMapCollidingStringInt(): ImperialMutableMap<CollidingString, Int>

    @Test
    fun `test contains when empty`() {
        val map = emptyCustomMutableMapStringInt()
        assertFalse(map.contains("Hello"))
    }

    @Test
    fun `test remove when empty`() {
        val map = emptyCustomMutableMapStringInt()
        assertNull(map.remove("Hello"))
    }

    @Test
    fun `test size when empty`() {
        val map = emptyCustomMutableMapStringInt()
        assertEquals(0, map.size)
    }

    @Test
    fun `test contains after put`() {
        val map = emptyCustomMutableMapStringInt()
        map.put("Hello", 3)
        assertTrue(map.contains("Hello"))
    }

    @Test
    fun `test remove after put`() {
        val map = emptyCustomMutableMapStringInt()
        map.put("Hello", 3)
        assertNull(map.remove("World"))
        assertEquals(3, map.remove("Hello"))
    }

    @Test
    fun `test size after put`() {
        val map = emptyCustomMutableMapStringInt()
        map.put("Hello", 3)
        assertEquals(1, map.size)
    }

    @Test
    fun `test get returns null`() {
        val map = emptyCustomMutableMapStringInt()
        map.put("Hello", 3)
        map.put("World", 4)
        assertNull(map["You"])
    }

    @Test
    fun `test get returns latest value`() {
        val map = emptyCustomMutableMapStringInt()
        map["Hello"] = 3
        map["World"] = 4
        map["Hello"] = 10
        map["Hello"] = 11
        assertEquals(11, map["Hello"])
    }

    @Test
    fun `test entries initially empty`() {
        val map = emptyCustomMutableMapStringInt()
        for (e in map) {
            fail("Map entries should be empty")
        }
    }

    @Test
    fun `test entries after some putting`() {
        val map = emptyCustomMutableMapStringInt()
        val entries =
            (1..100).map {
                ImperialMutableMap.Entry(it.toString(), it)
            }
        entries.forEach {
            map.put(it.key, it.value)
        }
        assertEquals(entries.size, map.size)
        assertEquals(entries, map.toSet().sortedBy { it.value })
    }

    @Test
    fun `test entries after some setting`() {
        val map = emptyCustomMutableMapStringInt()
        val expected: List<ImperialMutableMap.Entry<String, Int>> =
            (1..100).map {
                ImperialMutableMap.Entry(it.toString(), it)
            }
        expected.forEach {
            map[it.key] = it.value
        }
        assertEquals(expected.size, map.size)
        assertEquals(expected, map.toList().sortedBy { it.value })
    }

    @Test
    fun `test entries after some putting, removing and setting`() {
        val map = createCustomMutableMapByPuttingRemovingAndSetting()
        val expected = createExpectedEntriesFromPuttingRemovingAndSetting()
        assertEquals(expected.size, map.size)
        assertEquals(expected, map.toList().sortedBy { it.value })
    }

    @Test
    fun `test entries after some putting (collision prone)`() {
        val map = emptyCustomMutableMapCollidingStringInt()
        val expected =
            (1..100).map {
                ImperialMutableMap.Entry(CollidingString(it.toString()), it)
            }
        expected.forEach {
            map.put(it.key, it.value)
        }
        assertEquals(expected.size, map.size)
        assertEquals(expected, map.toList().sortedBy { it.value })
    }

    @Test
    fun `test entries after some setting (collision prone)`() {
        val map = emptyCustomMutableMapCollidingStringInt()
        val expected =
            (1..100).map {
                ImperialMutableMap.Entry(CollidingString(it.toString()), it)
            }
        expected.forEach {
            map[it.key] = it.value
        }
        assertEquals(expected.size, map.size)
        assertEquals(expected, map.toList().sortedBy { it.value })
    }

    @Test
    fun `test entries after some putting, removing and setting (collision prone)`() {
        val map = createCollisionProneMapByPuttingRemovingAndSetting()
        val expected = createCollisionProneExpectedEntriesFromPuttingRemovingAndSetting()
        assertEquals(expected.size, map.size)
        assertEquals(expected, map.toList().sortedBy { it.value })
    }

    @Test
    fun `performance test 1`() {
        println("Performance test started.")
        val map = emptyCustomMutableMapStringInt()
        for (i in 0..<1000000) {
            if (i.mod(10000) == 0) {
                println("Added $i elements out of 1000000. These messages should fly by if performance is adequate.")
            }
            map.put(i.toString(), i)
        }
        assertEquals(1000000, map.size)
    }

    @Test
    fun `performance test 2`() {
        println("Performance test started.")
        val map = emptyCustomMutableMapCollidingStringInt()
        for (i in 0..<20000) {
            if (i.mod(100) == 0) {
                println("Added $i elements out of 20000. These messages should fly by if performance is adequate.")
            }
            map.put(CollidingString(i.toString()), i)
        }
        assertEquals(20000, map.size)
    }

    class CollidingString(
        val string: String,
    ) : Comparable<CollidingString> {
        override fun hashCode(): Int = 5

        override fun compareTo(other: CollidingString): Int = string.compareTo(other.string)

        override fun equals(other: Any?): Boolean {
            if (other is CollidingString) {
                return string == other.string
            }
            return false
        }
    }

    private fun createCustomMutableMapByPuttingRemovingAndSetting(): ImperialMutableMap<String, Int> {
        val map = emptyCustomMutableMapStringInt()
        for (i in 1..100) {
            assertFalse(map.contains(i.toString()))
            assertNull(map[i.toString()])
            assertNull(map.put(i.toString(), i))
        }
        for (i in 1..100) {
            assertTrue(map.contains(i.toString()))
            assertEquals(i, map[i.toString()])
            if (i % 2 == 0) {
                val previous = map.remove(i.toString())
                assertNotNull(previous)
                assertEquals(i, previous)
            }
        }
        for (i in 1..100) {
            if (i % 4 == 0) {
                assertNull(map[i.toString()])
                assertFalse(map.contains(i.toString()))
                assertNull(map.set(i.toString(), i))
            }
        }
        return map
    }

    private fun createExpectedEntriesFromPuttingRemovingAndSetting(): List<ImperialMutableMap.Entry<String, Int>> {
        val entries =
            (1..100)
                .map {
                    ImperialMutableMap.Entry(it.toString(), it)
                }.filter {
                    it.value % 2 != 0 || it.value % 4 == 0
                }
        return entries
    }

    private fun createCollisionProneMapByPuttingRemovingAndSetting(): ImperialMutableMap<CollidingString, Int> {
        val map = emptyCustomMutableMapCollidingStringInt()
        for (i in 1..100) {
            assertFalse(map.contains(CollidingString(i.toString())))
            assertNull(map[CollidingString(i.toString())])
            assertNull(map.put(CollidingString(i.toString()), i))
        }
        for (i in 1..100) {
            assertTrue(map.contains(CollidingString(i.toString())))
            assertEquals(i, map[CollidingString(i.toString())])
            if (i % 2 == 0) {
                val previous = map.remove(CollidingString(i.toString()))
                assertNotNull(previous)
                assertEquals(i, previous)
            }
        }
        for (i in 1..100) {
            if (i % 4 == 0) {
                assertNull(map.get(CollidingString(i.toString())))
                assertFalse(map.contains(CollidingString(i.toString())))
                assertNull(map.set(CollidingString(i.toString()), i))
            }
        }
        return map
    }

    private fun createCollisionProneExpectedEntriesFromPuttingRemovingAndSetting(): List<ImperialMutableMap.Entry<CollidingString, Int>> {
        val entries =
            (1..100)
                .map {
                    ImperialMutableMap.Entry(CollidingString(it.toString()), it)
                }.filter {
                    it.value % 2 != 0 || it.value % 4 == 0
                }
        return entries
    }
}
