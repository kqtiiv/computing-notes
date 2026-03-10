package tunes

import kotlin.test.Test
import kotlin.test.assertTrue

class GoodPracticesTestsStandardTune {
    @Test
    fun testClassIsFinal() {
        assertTrue(StandardTune::class.isFinal)
    }

    @Test
    fun testAllHelperMembersPrivate() {
        checkAllOtherMembersPrivate(
            StandardTune::class,
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
