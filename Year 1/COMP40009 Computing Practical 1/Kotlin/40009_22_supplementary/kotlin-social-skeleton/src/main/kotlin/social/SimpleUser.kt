package social

import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock

class SimpleUser(
    override val userName: String,
    override val yearOfBirth: Int,
    override val bio: String,
    val befriendingStrategy: (target: User, candidate: User) -> Boolean = ::standardStrategy,
) : User {
    override val lock: Lock = ReentrantLock()
    private val _currentFriends: MutableList<User> = mutableListOf()
    override val currentFriends: List<User>
        get() = _currentFriends.toList()

    init {
        require(yearOfBirth in 1900..2100)
    }

//    override fun considerFriendRequest(candidateFriend: User): Boolean =
//        if (currentFriends.any { it.userName == candidateFriend.userName }) {
//            false
//        } else {
//            _currentFriends.add(candidateFriend)
//            true
//        }

    override fun considerFriendRequest(candidateFriend: User): Boolean =
        if (befriendingStrategy(this, candidateFriend)) {
            _currentFriends.add(candidateFriend)
            true
        } else {
            false
        }

    override fun removeFriend(user: User): Boolean = _currentFriends.removeIf { it.userName == user.userName }

    override fun removeLongestStandingFriend(): User? =
        if (currentFriends.isEmpty()) {
            null
        } else {
            _currentFriends.removeAt(0)
        }
}
