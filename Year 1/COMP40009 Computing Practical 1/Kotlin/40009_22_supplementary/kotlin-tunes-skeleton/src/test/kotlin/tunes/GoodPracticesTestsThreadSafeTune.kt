package tunes

import kotlin.test.Test
import kotlin.test.assertTrue

class GoodPracticesTestsThreadSafeTune {
    @Test
    fun testClassIsFinal() {
        assertTrue(ThreadSafeTune::class.isFinal)
    }

    @Test
    fun testAllHelperMembersPrivate() {
        checkAllOtherMembersPrivate(
            ThreadSafeTune::class,
            "addNote",
            "notes",
            "iterator",
            "totalDuration",
            "equals",
            "hashCode",
            "toString",
        )
    }
}
