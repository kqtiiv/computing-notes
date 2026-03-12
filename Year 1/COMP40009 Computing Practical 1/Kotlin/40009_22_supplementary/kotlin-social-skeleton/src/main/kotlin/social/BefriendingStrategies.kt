package social

fun standardStrategy(
    targetUser: User,
    candidateUser: User,
): Boolean = !targetUser.hasFriend(candidateUser)

fun unfriendlyStrategy(
    targetUser: User,
    candidateUser: User,
): Boolean = false

fun limitOfFiveStrategy(
    targetUser: User,
    candidateUser: User,
): Boolean = standardStrategy(targetUser, candidateUser) && removeUntilFive(targetUser)

private fun removeUntilFive(targetUser: User): Boolean {
    while (targetUser.currentFriends.size >= 5) {
        targetUser.removeLongestStandingFriend()
    }
    return true
}

fun interestedInDogsStrategy(
    targetUser: User,
    candidateUser: User,
): Boolean =
    standardStrategy(targetUser, candidateUser) &&
        candidateUser.bio
            .lowercase()
            .split(" ")
            .contains("dog")
