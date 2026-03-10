package social

private const val INITIAL_BUCKETS = 8
private const val LOAD_FACTOR = 0.75

class HashMapLinked<K, V> : OrderedMap<K, V> {
    private class Node<K, V>(
        val key: K,
        var value: V,
        var prev: Node<K, V>?,
        var next: Node<K, V>? = null,
    )

    private var head: Node<K, V>? = null
    private var tail: Node<K, V>? = null

    private var buckets: MutableList<MutableList<Node<K, V>>>

    init {
        buckets = mutableListOf()
        for (i in 0..<INITIAL_BUCKETS) {
            buckets.add(mutableListOf())
        }
    }

    override var size = 0
        private set

    override val values: List<V>
        get() {
            val vals: MutableList<V> = mutableListOf()
            var cur: Node<K, V>? = head
            while (cur != null) {
                vals.add(cur.value)
                cur = cur.next
            }
            return vals
        }

    override fun containsKey(key: K): Boolean = getBucket(key).any { it.key == key }

    override fun remove(key: K): V? {
        val bucket = getBucket(key)
        val cur: Node<K, V> = bucket.find { it.key == key } ?: return null

        bucket.remove(cur)

        if (cur == head) head = cur.next
        if (cur == tail) tail = cur.prev
        cur.prev?.next = cur.next
        cur.next?.prev = cur.prev
        size--
        return cur.value
    }

    override operator fun set(
        key: K,
        value: V,
    ): V? {
        val removed: V? = remove(key)

        val newNode: Node<K, V> = Node(key, value, tail)
        getBucket(key).add(newNode)

        if (head == null) head = newNode

        tail?.next = newNode
        tail = newNode

        size++

        resize()

        return removed
    }

    override fun removeLongestStandingEntry(): Pair<K, V>? =
        head?.let { oldHead ->
            remove(oldHead.key)?.let { oldHead.key to it }
        }

    private fun getBucket(key: K) = buckets[key.hashCode().mod(buckets.size)]

    private fun resize() {
        if (size <= LOAD_FACTOR * buckets.size) {
            return
        }
        val allContent = mutableListOf<Node<K, V>>()
        for (bucket in buckets) {
            allContent.addAll(bucket)
        }

        val newNumBuckets = buckets.size * 2

        buckets = mutableListOf()
        for (i in 0..<newNumBuckets) {
            buckets.add(mutableListOf())
        }

        for (node in allContent) {
            getBucket(node.key).add(node)
        }
    }
}
