package collections

// Useful imports that you will probably need:
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

class StripedHashmap<K, V>(
    private val bucketFactory: () -> Bucket<K, V>,
) : ImperialMutableMap<K, V> {
    private var buckets: Array<Bucket<K, V>> = Array(DEFAULT_BUCKET_SIZE) { bucketFactory() }

    private val locks: Array<Lock> = Array(DEFAULT_BUCKET_SIZE) { ReentrantLock() }

    private val _size = AtomicInteger()

    override val size: Int
        get() = _size.get()

    override fun iterator(): Iterator<ImperialMutableMap.Entry<K, V>> = toSinglyLinkedList().iterator()

    override fun put(
        key: K,
        value: V,
    ): V? {
        locks[key.lockIndex()].withLock {
            val bucket = key.bucket()
            for (entry in bucket) {
                if (entry.key == key) {
                    val result = entry.value
                    entry.value = value
                    return result
                }
            }
            _size.incrementAndGet()
            bucket.add(0, ImperialMutableMap.Entry(key, value))
        }

        if (size > buckets.size * MAX_LOAD_FACTOR) {
            resize()
        }

        return null
    }

    override fun get(key: K): V? =
        locks[key.lockIndex()].withLock {
            for (entry in key.bucket()) {
                if (entry.key == key) {
                    return entry.value
                }
            }
            return null
        }

    override fun remove(key: K): V? =
        locks[key.lockIndex()].withLock {
            val bucket = key.bucket()

            for ((index, entry) in bucket.withIndex()) {
                if (entry.key == key) {
                    val result = entry.value
                    bucket.removeAt(index)
                    _size.decrementAndGet()
                    return result
                }
            }
            return null
        }

    private fun K.bucketIndex(): Int = Math.floorMod(hashCode(), buckets.size)

    private fun K.bucket(): Bucket<K, V> = buckets[bucketIndex()]

    private fun K.lockIndex(): Int = Math.floorMod(hashCode(), DEFAULT_BUCKET_SIZE)

    private fun resize() {
        try {
            locks.forEach { it.lock() }
            if (size > buckets.size * MAX_LOAD_FACTOR) {
                val entries = toSinglyLinkedList()
                buckets = Array(buckets.size * 2) { bucketFactory() }
                entries.forEach {
                    _size.decrementAndGet()
                    put(it.key, it.value)
                }
            }
        } finally {
            locks.forEach { it.unlock() }
        }
    }

    private fun toSinglyLinkedList(): SinglyLinkedList<ImperialMutableMap.Entry<K, V>> {
        val entries = SinglyLinkedList<ImperialMutableMap.Entry<K, V>>()
        try {
            locks.forEach { it.lock() }
            for (bucket in buckets) {
                for (entry in bucket) {
                    entries.add(0, entry)
                }
            }
        } finally {
            locks.forEach { it.unlock() }
        }
        return entries
    }
}
