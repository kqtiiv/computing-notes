package social

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertSame
import kotlin.test.assertTrue
import kotlin.test.fail

class Question6Tests {
    @Test
    fun `test userName`() {
        assertEquals("xdc145", makeUserA().userName)
    }

    @Test
    fun `test yearOfBirth`() {
        assertEquals(1993, makeUserB().yearOfBirth)
    }

    @Test
    fun `test yearOfBirth exception 1`() {
        try {
            OptimisedUser("name", 1, "bio")
            fail("An IllegalArgumentException was expected.")
        } catch (_: IllegalArgumentException) {
            // Good: exception expected.
        }
    }

    @Test
    fun `test yearOfBirth exception 2`() {
        try {
            OptimisedUser("name", 2101, "bio")
            fail("An IllegalArgumentException was expected.")
        } catch (_: IllegalArgumentException) {
            // Good: exception expected.
        }
    }

    @Test
    fun `test yearOfBirth no exception 1`() {
        OptimisedUser("name", 1900, "bio")
    }

    @Test
    fun `test yearOfBirth no exception 2`() {
        OptimisedUser("name", 1900, "bio")
    }

    @Test
    fun `test bio`() {
        assertEquals("Looks like a cat but works like a dog", makeUserC().bio)
    }

    @Test
    fun `test currentFriends 1`() {
        val user1 = makeUserA()
        val user2 = makeUserB()
        val user3 = makeUserC()
        val user4 = makeUserD()
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
        val user1 = makeUserA()
        val user2 = makeUserB()
        val user3 = makeUserC()
        val user4 = makeUserD()
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
        val user1 = makeUserA()
        val user2 = makeUserB()
        val user3 = makeUserC()
        val user4 = makeUserD()
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
        val user1 = makeUserA()
        val user2 = makeUserB()
        val user3 = makeUserC()
        val user4 = makeUserD()
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

    @Test
    fun `test standard strategy`() {
        val user1 = makeUser1()
        val user2 = makeUser2()
        val user3 = makeUser3()
        val user4 = makeUser4()
        val user5 = makeUser5()
        val imposter = OptimisedUser(userName = user2.userName, yearOfBirth = 1910, bio = "Cthulu")
        assertTrue(user1.considerFriendRequest(user2))
        assertFalse(user1.considerFriendRequest(imposter))
        assertTrue(user1.considerFriendRequest(user3))
        assertTrue(user1.considerFriendRequest(user4))
        assertTrue(user1.considerFriendRequest(user5))
        assertEquals(listOf(user2, user3, user4, user5), user1.currentFriends)
    }

    @Test
    fun `test unfriendly strategy`() {
        val user1 = makeUser1()
        val user2 = makeUser2()
        val user3 = makeUser3()
        val user4 = makeUser4()
        val user5 = makeUser5()
        assertFalse(user4.considerFriendRequest(user1))
        assertFalse(user4.considerFriendRequest(user2))
        assertFalse(user4.considerFriendRequest(user3))
        assertFalse(user4.considerFriendRequest(user5))
        assertEquals(emptyList(), user4.currentFriends)
    }

    @Test
    fun `test limit of five strategy`() {
        val user1 = makeUser1()
        val user2 = makeUser2()
        val user3 = makeUser3()
        val user4 = makeUser4()
        val user5 = makeUser5()
        val user6 = makeUser6()
        val user7 = makeUser7()

        assertTrue(user2.considerFriendRequest(user1))
        assertEquals(listOf(user1), user2.currentFriends)
        val imposter = OptimisedUser(userName = user1.userName, yearOfBirth = 1981, bio = "Hello.")

        assertTrue(user2.considerFriendRequest(user3))
        assertEquals(listOf(user1, user3), user2.currentFriends)

        assertTrue(user2.considerFriendRequest(user4))
        assertEquals(listOf(user1, user3, user4), user2.currentFriends)

        assertTrue(user2.considerFriendRequest(user5))
        assertEquals(listOf(user1, user3, user4, user5), user2.currentFriends)

        assertTrue(user2.considerFriendRequest(user6))
        assertEquals(listOf(user1, user3, user4, user5, user6), user2.currentFriends)

        assertTrue(user2.considerFriendRequest(user7))
        assertEquals(listOf(user3, user4, user5, user6, user7), user2.currentFriends)

        assertTrue(user2.considerFriendRequest(imposter))
        assertEquals(listOf(user4, user5, user6, user7, imposter), user2.currentFriends)
    }

    @Test
    fun `test interested in dogs strategy`() {
        val user1 = makeUser1()
        val user2 = makeUser2()
        val user3 = makeUser3()
        val user4 = makeUser4()
        val user5 = makeUser5()
        val user8 = makeUser8()
        val imposter = OptimisedUser(userName = user5.userName, yearOfBirth = 1974, bio = user5.bio)
        assertFalse(user8.considerFriendRequest(user1))
        assertFalse(user8.considerFriendRequest(user2))
        assertTrue(user8.considerFriendRequest(user3))
        assertFalse(user8.considerFriendRequest(user4))
        assertTrue(user8.considerFriendRequest(user5))
        assertFalse(user8.considerFriendRequest(imposter))
        assertEquals(listOf(user3, user5), user8.currentFriends)
    }

    private fun makeUser1(): User =
        OptimisedUser(
            userName = "xdc145",
            yearOfBirth = 1990,
            bio = "Born in Devon, loves dogs",
            befriendingStrategy = ::standardStrategy,
        )

    private fun makeUser2(): User =
        OptimisedUser(
            userName = "gdc1645",
            yearOfBirth = 1993,
            bio = "Born in Dorset, loves cats",
            befriendingStrategy = ::limitOfFiveStrategy,
        )

    private fun makeUser3(): User =
        OptimisedUser(
            userName = "geeman2000",
            yearOfBirth = 1996,
            bio = "Looks like a cat but works like a dog",
            befriendingStrategy = ::interestedInDogsStrategy,
        )

    private fun makeUser4(): User =
        OptimisedUser(
            userName = "gerald_the_unfriendly",
            yearOfBirth = 2002,
            bio = "Leave me alone",
            befriendingStrategy = ::unfriendlyStrategy,
        )

    private fun makeUser5(): User =
        OptimisedUser(
            userName = "mary_hope",
            yearOfBirth = 2003,
            bio = "A Dog Is For Life, Not For Christmas",
            befriendingStrategy = ::standardStrategy,
        )

    private fun makeUser6(): User =
        OptimisedUser(
            userName = "dopey",
            yearOfBirth = 2004,
            bio = "Bored out of my mind",
            befriendingStrategy = ::unfriendlyStrategy,
        )

    private fun makeUser7(): User =
        OptimisedUser(
            userName = "dippy",
            yearOfBirth = 2004,
            bio = "Stone cold crazy",
            befriendingStrategy = ::standardStrategy,
        )

    private fun makeUser8(): User =
        OptimisedUser(
            userName = "roger_brian_john_freddie",
            yearOfBirth = 2005,
            bio = "A massive fan of Queen.",
            befriendingStrategy = ::interestedInDogsStrategy,
        )

    private fun makeUserA(): User =
        OptimisedUser(
            userName = "xdc145",
            yearOfBirth = 1990,
            bio = "Born in Devon, loves dogs",
        )

    private fun makeUserB(): User =
        OptimisedUser(
            userName = "gdc1645",
            yearOfBirth = 1993,
            bio = "Born in Dorset, loves cats",
        )

    private fun makeUserC(): User =
        OptimisedUser(
            userName = "geeman2000",
            yearOfBirth = 1996,
            bio = "Looks like a cat but works like a dog",
        )

    private fun makeUserD(): User =
        OptimisedUser(
            userName = "gerald_the_unfriendly",
            yearOfBirth = 2002,
            bio = "Leave me alone",
        )
}
