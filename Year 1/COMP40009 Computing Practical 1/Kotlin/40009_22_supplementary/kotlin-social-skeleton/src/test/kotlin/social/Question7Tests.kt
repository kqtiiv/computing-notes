package social

import java.util.Random
import kotlin.test.Test
import kotlin.test.assertEquals

class Question7Tests {
    @Test
    fun `test match maker basic`() {
        val matchmaker = Matchmaker(::bornInSameYear)
        val user1 =
            SimpleUser(
                userName = "ungoliant",
                yearOfBirth = 1900,
                bio = "spidery",
            )
        val user2 =
            SimpleUser(
                userName = "MrMelchor",
                yearOfBirth = 1900,
                bio = "wizardy",
            )
        val user3 =
            SimpleUser(
                userName = "Sauron",
                yearOfBirth = 1964,
                bio = "mordorish",
            )
        matchmaker.tryMatching(user1, user2)
        assertEquals(listOf(user2), user1.currentFriends)
        assertEquals(listOf(user1), user2.currentFriends)
        matchmaker.tryMatching(user1, user3)
        matchmaker.tryMatching(user3, user2)
        assertEquals(listOf(user2), user1.currentFriends)
        assertEquals(listOf(user1), user2.currentFriends)
    }

    @Test
    fun `test match maker same user`() {
        val matchmaker = Matchmaker(::bornInSameYear)
        val user1 =
            SimpleUser(
                userName = "ungoliant",
                yearOfBirth = 1900,
                bio = "spidery",
            )
        matchmaker.tryMatching(user1, user1)
        assertEquals(listOf(), user1.currentFriends)
    }

    @Test
    fun `test match maker concurrent`() {
        for (repeat in 1..100) {
            val users =
                (0..<20).map {
                    SimpleUser(
                        userName = "user$it",
                        yearOfBirth =
                            if (it.mod(2) == 0) {
                                1990
                            } else {
                                1991
                            },
                        bio = "I was born...",
                    )
                }

            val matchmakerThreadBody =
                object : Runnable {
                    override fun run() {
                        val matchmaker = Matchmaker(::bornInSameYear)
                        val rng = Random()
                        for (i in 0..1000) {
                            matchmaker.tryMatching(users[rng.nextInt(users.size)], users[rng.nextInt(users.size)])
                        }
                    }
                }

            val threads = (0..<4).map { Thread(matchmakerThreadBody) }
            threads.forEach(Thread::start)
            threads.forEach(Thread::join)

            for (user in users) {
                for (friend in user.currentFriends) {
                    assertEquals(user.yearOfBirth, friend.yearOfBirth)
                }
            }
        }
    }
}

private fun bornInSameYear(
    first: User,
    second: User,
): Boolean = first.yearOfBirth == second.yearOfBirth
