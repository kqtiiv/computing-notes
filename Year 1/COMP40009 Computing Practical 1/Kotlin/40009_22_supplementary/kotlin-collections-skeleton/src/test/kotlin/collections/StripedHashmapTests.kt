package collections

import kotlin.test.Test
import kotlin.test.assertFalse

class StripedHashmapTests : ThreadSafeImperialMutableMapTestsParent() {
    override fun emptyThreadSafeMapIntString(): ImperialMutableMap<Int, String> = StripedHashmap(::SinglyLinkedList)

    override fun emptyCustomMutableMapStringInt(): ImperialMutableMap<String, Int> = StripedHashmap(::SinglyLinkedList)

    override fun emptyCustomMutableMapCollidingStringInt(): ImperialMutableMap<CollidingString, Int> = StripedHashmap(::SinglyLinkedList)

    // This test is present merely to ensure that at least one concrete test case exists.
    // The real tests are inherited from the abstract superclass.
    @Test
    fun `trivial test`() {
        assertFalse(emptyCustomMutableMapStringInt().iterator().hasNext())
    }
}
