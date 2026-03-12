package social

import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock

class SimpleUser(
    override val userName: String,
    override val yearOfBirth: Int,
    override val bio: String,
    val befriendingStrategy: (User, User) -> Boolean = ::standardStrategy,
) : User {
    override val lock: Lock = ReentrantLock()
    private val _currentFriends: MutableList<User> = mutableListOf()
    override val currentFriends: List<User>
        get() = _currentFriends.toList()

    init {
        require(yearOfBirth in MIN_YEAR_OF_BIRTH..MAX_YEAR_OF_BIRTH)
    }

    override fun considerFriendRequest(candidateFriend: User): Boolean =
        befriendingStrategy(this, candidateFriend) && _currentFriends.add(candidateFriend)

    override fun removeFriend(user: User): Boolean = _currentFriends.removeIf { it.userName == user.userName }

    override fun removeLongestStandingFriend(): User? = _currentFriends.removeFirstOrNull()
}
