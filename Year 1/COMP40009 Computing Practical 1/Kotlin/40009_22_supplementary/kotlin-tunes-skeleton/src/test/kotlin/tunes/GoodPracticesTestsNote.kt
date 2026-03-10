package tunes

import kotlin.test.Test
import kotlin.test.assertTrue

class GoodPracticesTestsNote {
    @Test
    fun testClassIsFinal() {
        assertTrue(Note::class.isFinal)
    }

    @Test
    fun testAllMembersFinal() {
        checkAllOtherMembersFinal(
            Note::class,
            "equals",
            "hashCode",
            "toString",
        )
    }

    @Test
    fun testAllHelperMembersPrivate() {
        checkAllOtherMembersPrivate(
            Note::class,
            "equals",
            "toString",
            "hashCode",
            "hasNoteAbove",
            "hasNoteBelow",
            "noteAbove",
            "noteBelow",
            "duration",
            "pitch",
            "copy",
            "component1",
            "component2",
        )
    }
}
