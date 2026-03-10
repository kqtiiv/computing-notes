package tunes

import kotlin.test.Test
import kotlin.test.assertTrue

class GoodPracticesTestsTransposedTune {
    @Test
    fun testClassIsFinal() {
        assertTrue(TransposedTune::class.isFinal)
    }

    @Test
    fun testAllHelperMembersPrivate() {
        checkAllOtherMembersPrivate(
            TransposedTune::class,
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
