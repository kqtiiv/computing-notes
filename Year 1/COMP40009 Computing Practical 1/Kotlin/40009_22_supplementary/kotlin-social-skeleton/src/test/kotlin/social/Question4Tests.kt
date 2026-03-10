package social

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class Question4Tests {
    @Test
    fun `test standard strategy`() {
        val user1 = makeUser1()
        val user2 = makeUser2()
        val user3 = makeUser3()
        val user4 = makeUser4()
        val user5 = makeUser5()
        val imposter = SimpleUser(userName = user2.userName, yearOfBirth = 1910, bio = "Cthulu")
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
        val imposter = SimpleUser(userName = user1.userName, yearOfBirth = 1981, bio = "Hello.")

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
        val imposter = SimpleUser(userName = user5.userName, yearOfBirth = 1974, bio = user5.bio)
        assertFalse(user8.considerFriendRequest(user1))
        assertFalse(user8.considerFriendRequest(user2))
        assertTrue(user8.considerFriendRequest(user3))
        assertFalse(user8.considerFriendRequest(user4))
        assertTrue(user8.considerFriendRequest(user5))
        assertFalse(user8.considerFriendRequest(imposter))
        assertEquals(listOf(user3, user5), user8.currentFriends)
    }

    private fun makeUser1(): User =
        SimpleUser(
            userName = "xdc145",
            yearOfBirth = 1990,
            bio = "Born in Devon, loves dogs",
            befriendingStrategy = ::standardStrategy,
        )

    private fun makeUser2(): User =
        SimpleUser(
            userName = "gdc1645",
            yearOfBirth = 1993,
            bio = "Born in Dorset, loves cats",
            befriendingStrategy = ::limitOfFiveStrategy,
        )

    private fun makeUser3(): User =
        SimpleUser(
            userName = "geeman2000",
            yearOfBirth = 1996,
            bio = "Looks like a cat but works like a dog",
            befriendingStrategy = ::interestedInDogsStrategy,
        )

    private fun makeUser4(): User =
        SimpleUser(
            userName = "gerald_the_unfriendly",
            yearOfBirth = 2002,
            bio = "Leave me alone",
            befriendingStrategy = ::unfriendlyStrategy,
        )

    private fun makeUser5(): User =
        SimpleUser(
            userName = "mary_hope",
            yearOfBirth = 2003,
            bio = "A Dog Is For Life, Not For Christmas",
            befriendingStrategy = ::standardStrategy,
        )

    private fun makeUser6(): User =
        SimpleUser(
            userName = "dopey",
            yearOfBirth = 2004,
            bio = "Bored out of my mind",
            befriendingStrategy = ::unfriendlyStrategy,
        )

    private fun makeUser7(): User =
        SimpleUser(
            userName = "dippy",
            yearOfBirth = 2004,
            bio = "Stone cold crazy",
            befriendingStrategy = ::standardStrategy,
        )

    private fun makeUser8(): User =
        SimpleUser(
            userName = "roger_brian_john_freddie",
            yearOfBirth = 2005,
            bio = "A massive fan of Queen.",
            befriendingStrategy = ::interestedInDogsStrategy,
        )
}
