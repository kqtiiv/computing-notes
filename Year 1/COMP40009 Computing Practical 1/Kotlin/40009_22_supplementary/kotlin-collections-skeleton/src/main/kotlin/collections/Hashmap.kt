package collections

class Hashmap<K, V>(
    private val bucketFactory: () -> Bucket<K, V>,
) : ImperialMutableMap<K, V> {
    // Problem:  The size of the hashmap is not altered when an element is removed
    // Solution: Decrement the size on removal

    // Problem:  Hashmap is only resized if the entry already exists in the map
    // but not when the size of the hashmap increases, so it is not resized, which can cause more collisions or can
    // cause the number of elements to overflow
    // Solution: Move the resizing check to after the size is incremented

    // Problem:  In the 'put' function, when looping through a bucket, it references it's entries using indexing,
    // which is inefficient, as it is a singly linked list, so each index call is O(N)
    // Solution: Use an iterator

    // Problem:  When the resize function is called, the put function is called, which adds 1 to the size each time
    // Solution: Set size to 0 when resizing

    // Problem:  The method name 'helperFunction' is not descriptive enough
    // Solution: Change the name to something more meaningful, ie what it does.

    // Problem:  The helper functions can be accessed from outside the class, as they are public, reducing encapsulation
    // Solution: Make them private

    // Problem:  The size can be changed from outside the class
    // Solution: Make it private set

    // Problem:  Buckets can be accessed and modified from outside the class
    // Solution: Make it private

    // Problem:  16 is a magic number
    // Solution: Extract it into a constant

    private var buckets: Array<Bucket<K, V>> = Array(DEFAULT_BUCKET_SIZE) { bucketFactory() }

    override var size: Int = 0
        private set

    // The iterator implementation works by putting all entries of the map into a list, and then returns an iterator to
    // this list. This is simpler than the "on demand" iterator that you implemented during a lab exercise on hashmaps.
    // This simpler approach is intentional here; it is not one of the problems you are supposed to identify.
    override fun iterator(): Iterator<ImperialMutableMap.Entry<K, V>> = toSinglyLinkedList().iterator()

    override fun put(
        key: K,
        value: V,
    ): V? {
        val bucket = key.bucket()
        for (entry in bucket) {
            if (entry.key == key) {
                val result = entry.value
                entry.value = value
                return result
            }
        }
        size++
        bucket.add(0, ImperialMutableMap.Entry(key, value))

        if (size > buckets.size * MAX_LOAD_FACTOR) {
            resize()
        }

        return null
    }

    override fun get(key: K): V? {
        for (entry in key.bucket()) {
            if (entry.key == key) {
                return entry.value
            }
        }
        return null
    }

    override fun remove(key: K): V? {
        val bucket = key.bucket()
        for ((index, entry) in bucket.withIndex()) {
            if (entry.key == key) {
                val result = entry.value
                bucket.removeAt(index)
                size--
                return result
            }
        }
        return null
    }

    private fun K.bucketIndex(): Int = Math.floorMod(hashCode(), buckets.size)

    private fun K.bucket(): Bucket<K, V> = buckets[bucketIndex()]

    private fun resize() {
        val entries = toSinglyLinkedList()
        size = 0
        buckets = Array(buckets.size * 2) { bucketFactory() }
        entries.forEach {
            put(it.key, it.value)
        }
    }

    private fun toSinglyLinkedList(): SinglyLinkedList<ImperialMutableMap.Entry<K, V>> {
        val entries = SinglyLinkedList<ImperialMutableMap.Entry<K, V>>()
        for (bucket in buckets) {
            for (entry in bucket) {
                entries.add(0, entry)
            }
        }
        return entries
    }
}
