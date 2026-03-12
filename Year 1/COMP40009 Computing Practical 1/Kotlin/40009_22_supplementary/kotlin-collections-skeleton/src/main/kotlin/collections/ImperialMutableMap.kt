package collections

interface ImperialMutableMap<K, V> : Iterable<ImperialMutableMap.Entry<K, V>> {
    data class Entry<K, V>(
        val key: K,
        var value: V,
    )

    // Yields the number of entries in the map.
    val size: Int

    // Provides an iterator for traversing the entries of the map. This method is
    // required by Iterable.
    override operator fun iterator(): Iterator<Entry<K, V>>

    // Returns the value at 'key', or null if there is no such value.
    // This operator allows array-like indexing.
    operator fun get(key: K): V?

    // Operator version of 'put' to allow array-like indexing.
    operator fun set(
        key: K,
        value: V,
    ): V? = put(key, value)

    // Associates 'value' with 'key'. Returns the previous value associated with
    // 'key', or null if there is no such previous value.
    fun put(
        key: K,
        value: V,
    ): V?

    // Removes entry with key 'key' from the map if such an entry exists, returning
    // the associated value if so. Otherwise, returns null.
    fun remove(key: K): V?

    // Returns true if and only if there is some value associated with 'key' in the map.
    fun contains(key: K): Boolean = get(key) != null
}
