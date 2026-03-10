package social

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertSame
import kotlin.test.assertTrue
import kotlin.test.fail

class Question2Tests {
    @Test
    fun `test userName`() {
        assertEquals("xdc145", makeUser1().userName)
    }

    @Test
    fun `test yearOfBirth`() {
        assertEquals(1993, makeUser2().yearOfBirth)
    }

    @Test
    fun `test yearOfBirth exception 1`() {
        try {
            SimpleUser("name", 1, "bio")
            fail("An IllegalArgumentException was expected.")
        } catch (_: IllegalArgumentException) {
            // Good: exception expected.
        }
    }

    @Test
    fun `test yearOfBirth exception 2`() {
        try {
            SimpleUser("name", 2101, "bio")
            fail("An IllegalArgumentException was expected.")
        } catch (_: IllegalArgumentException) {
            // Good: exception expected.
        }
    }

    @Test
    fun `test yearOfBirth no exception 1`() {
        SimpleUser("name", 1900, "bio")
    }

    @Test
    fun `test yearOfBirth no exception 2`() {
        SimpleUser("name", 2100, "bio")
    }

    @Test
    fun `test bio`() {
        assertEquals("Looks like a cat but works like a dog", makeUser3().bio)
    }

    @Test
    fun `test currentFriends 1`() {
        val user1 = makeUser1()
        val user2 = makeUser2()
        val user3 = makeUser3()
        val user4 = makeUser4()
        assertTrue(user1.currentFriends.isEmpty())
        user1.considerFriendRequest(user2)
        assertTrue(user1.hasFriend(user2))
        assertEquals(listOf(user2), user1.currentFriends)
        user1.considerFriendRequest(user4)
        assertTrue(user1.hasFriend(user2))
        assertTrue(user1.hasFriend(user4))
        assertEquals(listOf(user2, user4), user1.currentFriends)
        user1.considerFriendRequest(user3)
        assertTrue(user1.hasFriend(user2))
        assertTrue(user1.hasFriend(user4))
        assertTrue(user1.hasFriend(user3))
        assertEquals(listOf(user2, user4, user3), user1.currentFriends)
        user1.considerFriendRequest(user2)
        assertTrue(user1.hasFriend(user2))
        assertTrue(user1.hasFriend(user4))
        assertTrue(user1.hasFriend(user3))
        assertEquals(listOf(user2, user4, user3), user1.currentFriends)
    }

    @Test
    fun `test currentFriends 2`() {
        val user1 = makeUser1()
        val user2 = makeUser2()
        val user3 = makeUser3()
        val user4 = makeUser4()
        assertTrue(user1.currentFriends.isEmpty())
        user1.considerFriendRequest(user2)
        assertEquals(listOf(user2), user1.currentFriends)
        user1.considerFriendRequest(user4)
        assertEquals(listOf(user2, user4), user1.currentFriends)

        val friendsSnapshot = user1.currentFriends
        assertEquals(listOf(user2, user4), friendsSnapshot)

        user1.considerFriendRequest(user3)
        assertEquals(listOf(user2, user4, user3), user1.currentFriends)
        assertEquals(listOf(user2, user4), friendsSnapshot)

        user1.considerFriendRequest(user2)
        assertEquals(listOf(user2, user4, user3), user1.currentFriends)
        assertEquals(listOf(user2, user4), friendsSnapshot)
    }

    @Test
    fun `test removeFriend`() {
        val user1 = makeUser1()
        val user2 = makeUser2()
        val user3 = makeUser3()
        val user4 = makeUser4()
        user1.considerFriendRequest(user2)
        user1.considerFriendRequest(user4)
        user1.considerFriendRequest(user3)
        user1.considerFriendRequest(user2)
        assertEquals(listOf(user2, user4, user3), user1.currentFriends)
        assertTrue(user1.removeFriend(user2))
        assertEquals(listOf(user4, user3), user1.currentFriends)
        assertFalse(user1.removeFriend(user2))
        assertTrue(user1.removeFriend(user3))
        assertEquals(listOf(user4), user1.currentFriends)
        assertFalse(user1.removeFriend(user3))
        assertTrue(user1.removeFriend(user4))
        assertEquals(emptyList(), user1.currentFriends)
        assertFalse(user1.removeFriend(user4))
    }

    @Test
    fun `test removeLongestStandingFriend`() {
        val user1 = makeUser1()
        val user2 = makeUser2()
        val user3 = makeUser3()
        val user4 = makeUser4()
        user1.considerFriendRequest(user2)
        user1.considerFriendRequest(user4)
        user1.considerFriendRequest(user3)
        user1.considerFriendRequest(user2)
        assertEquals(listOf(user2, user4, user3), user1.currentFriends)
        assertSame(user2, user1.removeLongestStandingFriend())
        assertEquals(listOf(user4, user3), user1.currentFriends)
        assertSame(user4, user1.removeLongestStandingFriend())
        assertEquals(listOf(user3), user1.currentFriends)
        assertSame(user3, user1.removeLongestStandingFriend())
        assertEquals(emptyList(), user1.currentFriends)
        assertNull(user1.removeLongestStandingFriend())
    }

    private fun makeUser1(): User =
        SimpleUser(
            userName = "xdc145",
            yearOfBirth = 1990,
            bio = "Born in Devon, loves dogs",
        )

    private fun makeUser2(): User =
        SimpleUser(
            userName = "gdc1645",
            yearOfBirth = 1993,
            bio = "Born in Dorset, loves cats",
        )

    private fun makeUser3(): User =
        SimpleUser(
            userName = "geeman2000",
            yearOfBirth = 1996,
            bio = "Looks like a cat but works like a dog",
        )

    private fun makeUser4(): User =
        SimpleUser(
            userName = "gerald_the_unfriendly",
            yearOfBirth = 2002,
            bio = "Leave me alone",
        )
}
