package social

import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock

class OptimisedUser(
    override val userName: String,
    override val yearOfBirth: Int,
    override val bio: String,
    val befriendingStrategy: (target: User, candidate: User) -> Boolean = ::standardStrategy,
) : User {
    private val _currentFriends: HashMapLinked<String, User> = HashMapLinked()
    override val currentFriends: List<User>
        get() = _currentFriends.values

    override val lock: Lock = ReentrantLock()

    init {
        require(yearOfBirth in 1900..2100)
    }

    override fun considerFriendRequest(candidateFriend: User): Boolean =
        if (befriendingStrategy(this, candidateFriend)) {
            _currentFriends[candidateFriend.userName] = candidateFriend
            true
        } else {
            false
        }

    override fun removeFriend(user: User): Boolean = _currentFriends.remove(user.userName) != null

    override fun removeLongestStandingFriend(): User? = _currentFriends.removeLongestStandingEntry()?.second
}
