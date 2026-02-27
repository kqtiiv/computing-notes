package ImperialMutableList

interface ImperialMutableList<T> {
    // these are called abstract methods
    // they describe the services that the mutable list PROMISES WILL PROVIDE

    // val means that read access must be provided
    // the size might change (due to the add and remove calls)
    // but the CLIENT should not be able to change the size directly, hence val
    val size: Int

    operator fun get(index: Int): T

    fun add(
        index: Int,
        element: T,
    )

    fun add(element: T)

    fun clear()

    fun contains(element: T): Boolean

    fun remove(element: T): Boolean

    fun removeAt(index: Int): T

    fun isEmpty(): Boolean = size == 0

    operator fun set(
        index: Int,
        element: T,
    )

    operator fun iterator(): Iterator<T>

    fun addAll(other: ImperialMutableList<T>) {
        for (element in other) {
            add(element)
        }
    }
}
