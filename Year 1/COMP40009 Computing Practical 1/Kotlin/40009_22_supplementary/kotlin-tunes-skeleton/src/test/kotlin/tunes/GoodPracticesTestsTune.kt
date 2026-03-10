package tunes

import kotlin.test.Test

class GoodPracticesTestsTune {
    @Test
    fun testAllHelperMembersPrivate() {
        checkAllOtherMembersPrivate(
            Tune::class,
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
