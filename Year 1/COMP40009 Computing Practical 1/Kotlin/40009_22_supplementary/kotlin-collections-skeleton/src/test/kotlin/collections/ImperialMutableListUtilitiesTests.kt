package collections

import kotlin.test.Test
import kotlin.test.assertEquals

class ImperialMutableListUtilitiesTests {
    @Test
    fun `removeAll matching list types`() {
        val list1: ImperialMutableList<String> = SinglyLinkedList()
        list1.add(0, "a")
        list1.add(0, "b")
        list1.add(0, "c")
        list1.add(0, "d")
        list1.add(0, "e")
        val list2: ImperialMutableList<String> = SinglyLinkedList()
        list2.add(0, "a")
        list2.add(0, "b")
        list2.add(0, "c")
        list1.removeAll(list2)
        assertEquals(2, list1.size)
        assertEquals("e", list1[0])
        assertEquals("d", list1[1])
    }

    @Test
    fun `removeAll different list types`() {
        val list1: ImperialMutableList<Any> = SinglyLinkedList()
        list1.add(0, "a")
        list1.add(0, "b")
        list1.add(0, "c")
        list1.add(0, 1)
        list1.add(0, 2)
        list1.add(0, 3)
        val list2: ImperialMutableList<String> = SinglyLinkedList()
        list2.add(0, "a")
        list2.add(0, "b")
        list2.add(0, "c")
        list1.removeAll(list2)
        assertEquals(3, list1.size)
        assertEquals(3, list1[0])
        assertEquals(2, list1[1])
        assertEquals(1, list1[2])
    }
}
