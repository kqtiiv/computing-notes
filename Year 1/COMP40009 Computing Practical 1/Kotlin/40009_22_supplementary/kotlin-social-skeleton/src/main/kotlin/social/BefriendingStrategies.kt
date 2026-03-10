package social

import kotlin.collections.any

fun standardStrategy(
    targetUser: User,
    candidateUser: User,
): Boolean = !targetUser.currentFriends.any { it.userName == candidateUser.userName }

fun unfriendlyStrategy(
    targetUser: User,
    candidateUser: User,
): Boolean = false

fun limitOfFiveStrategy(
    targetUser: User,
    candidateUser: User,
): Boolean =
    if (standardStrategy(targetUser, candidateUser)) {
        while (targetUser.currentFriends.size >= 5) {
            targetUser.removeLongestStandingFriend()
        }
        true
    } else {
        false
    }

fun interestedInDogsStrategy(
    targetUser: User,
    candidateUser: User,
): Boolean =
    if (standardStrategy(targetUser, candidateUser)) {
        candidateUser.bio.lowercase().contains("dog")
    } else {
        false
    }
