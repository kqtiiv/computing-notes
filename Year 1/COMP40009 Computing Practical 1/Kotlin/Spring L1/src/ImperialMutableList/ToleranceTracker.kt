package ImperialMutableList

enum class ToleranceStatus {
    CAN_TOLERATE,
    CANNOT_TOLERATE,
    UNKNOWN,
}

class ToleranceTracker {
    private val canTolerate: MutableMap<String, MutableSet<String>> = mutableMapOf()

    operator fun get(
        person: String,
        otherPerson: String,
    ): ToleranceStatus =
        canTolerate[person]?.let { tolerates ->
            if (tolerates.contains(otherPerson)) {
                ToleranceStatus.CAN_TOLERATE
            } else {
                ToleranceStatus.CANNOT_TOLERATE
            }
        } ?: ToleranceStatus.UNKNOWN

    operator fun set(
        person: String,
        otherPerson: String,
        personToleratesOther: Boolean,
    ) {
        val toleratedByPerson: MutableSet<String> =
            canTolerate.getOrPut(person) { mutableSetOf() }
        if (personToleratesOther) {
            toleratedByPerson.add(otherPerson)
        } else {
            toleratedByPerson.remove(otherPerson)
        }
    }
}
