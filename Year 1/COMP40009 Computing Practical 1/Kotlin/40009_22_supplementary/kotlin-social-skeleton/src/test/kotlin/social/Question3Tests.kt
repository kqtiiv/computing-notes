package social

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class Question3Tests {
    @Test
    fun `test standard strategy`() {
        val user1 = makeUser1()
        val user2 = makeUser2()
        val user3 = makeUser3()
        val user4 = makeUser4()
        val user5 = makeUser5()
        val imposter = SimpleUser(userName = user2.userName, yearOfBirth = 1910, bio = "Cthulu")
        user1.considerFriendRequest(user2)
        assertFalse(standardStrategy(user1, imposter))
        assertTrue(standardStrategy(user1, user3))
        assertTrue(standardStrategy(user1, user4))
        assertTrue(standardStrategy(user1, user5))
    }

    @Test
    fun `test unfriendly strategy`() {
        val user1 = makeUser1()
        val user2 = makeUser2()
        val user3 = makeUser3()
        val user4 = makeUser4()
        val user5 = makeUser5()
        assertFalse(unfriendlyStrategy(user1, user2))
        assertFalse(unfriendlyStrategy(user1, user3))
        assertFalse(unfriendlyStrategy(user1, user4))
        assertFalse(unfriendlyStrategy(user1, user5))
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
        val user8 = makeUser8()
        val user9 = makeUser9()
        user1.considerFriendRequest(user2)
        user1.considerFriendRequest(user3)
        user1.considerFriendRequest(user4)
        user1.considerFriendRequest(user5)
        user1.considerFriendRequest(user6)
        user1.considerFriendRequest(user7)
        assertEquals(listOf(user2, user3, user4, user5, user6, user7), user1.currentFriends)
        val imposter = SimpleUser(userName = user2.userName, yearOfBirth = 1910, bio = "Cthulu")
        assertFalse(limitOfFiveStrategy(user1, imposter))
        assertEquals(listOf(user2, user3, user4, user5, user6, user7), user1.currentFriends)
        assertTrue(limitOfFiveStrategy(user1, user8))
        assertEquals(listOf(user4, user5, user6, user7), user1.currentFriends)
        user1.considerFriendRequest(user8)
        assertEquals(listOf(user4, user5, user6, user7, user8), user1.currentFriends)
        assertTrue(limitOfFiveStrategy(user1, user9))
        assertEquals(listOf(user5, user6, user7, user8), user1.currentFriends)
    }

    @Test
    fun `test interested in dogs strategy`() {
        val user1 = makeUser1()
        val user2 = makeUser2()
        val user3 = makeUser3()
        val user4 = makeUser4()
        val user5 = makeUser5()
        val user6 = makeUser6()
        val user7 = makeUser7()
        val user8 = makeUser8()
        val user9 = makeUser9()
        assertFalse(interestedInDogsStrategy(user1, user2))
        assertTrue(interestedInDogsStrategy(user1, user3))
        assertFalse(interestedInDogsStrategy(user1, user4))
        assertTrue(interestedInDogsStrategy(user1, user5))
        assertFalse(interestedInDogsStrategy(user1, user6))
        assertFalse(interestedInDogsStrategy(user1, user7))
        assertFalse(interestedInDogsStrategy(user1, user8))
        assertFalse(interestedInDogsStrategy(user1, user9))
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

    private fun makeUser5(): User =
        SimpleUser(
            userName = "mary_hope",
            yearOfBirth = 2003,
            bio = "A Dog Is For Life, Not For Christmas",
        )

    private fun makeUser6(): User =
        SimpleUser(
            userName = "dopey",
            yearOfBirth = 2004,
            bio = "Bored out of my mind",
        )

    private fun makeUser7(): User =
        SimpleUser(
            userName = "dippy",
            yearOfBirth = 2004,
            bio = "Stone cold crazy",
        )

    private fun makeUser8(): User =
        SimpleUser(
            userName = "roger_brian_john_freddie",
            yearOfBirth = 2005,
            bio = "A massive fan of Queen.",
        )

    private fun makeUser9(): User =
        SimpleUser(
            userName = "major_headache",
            yearOfBirth = 1969,
            bio = "I got my first real six string.",
        )
}
