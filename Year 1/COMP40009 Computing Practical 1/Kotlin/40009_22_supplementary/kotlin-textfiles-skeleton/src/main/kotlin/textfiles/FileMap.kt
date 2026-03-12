package textfiles

private typealias Entry = Pair<String, TextFile>
private typealias Bucket = MutableList<Entry>

private fun emptyBucket(): Bucket = mutableListOf()

private const val MAX_LOAD: Double = 0.75

private const val INIT_NUM_BUCKETS: Int = 4

class FileMap {
    private var buckets: List<Bucket> = List(INIT_NUM_BUCKETS) { emptyBucket() }

    var size: Int = 0
        private set

    operator fun get(fileName: String): TextFile? {
        val bucket: Bucket = buckets[fileName.bucketIndex()]
        for ((name, file) in bucket) {
            if (name == fileName) {
                return file
            }
        }
        return null
    }

    operator fun set(
        fileName: String,
        file: TextFile,
    ) {
        val targetBucket: Bucket = buckets[fileName.bucketIndex()]

        for (i in targetBucket.indices) {
            val (name, _) = targetBucket[i]
            if (name == fileName) {
                targetBucket[i] = fileName to file
                return
            }
        }

        // doesn't exist already
        size++
        targetBucket += fileName to file

        if (size > buckets.size * MAX_LOAD) {
            resize()
        }
    }

    private fun resize() {
        val oldBuckets: List<Bucket> = buckets
        buckets = List(2 * buckets.size) { emptyBucket() }

        for (bucket in oldBuckets) {
            for (file in bucket) {
                buckets[file.first.bucketIndex()] += file
            }
        }
    }

    private fun String.bucketIndex(): Int = Math.floorMod(this.hashCode(), buckets.size)

    operator fun iterator() =
        object : Iterator<Entry> {
            private var bucket: Int = 0
            private var entry: Int = 0

            init {
                nextBucket()
            }

            override fun hasNext(): Boolean = bucket < buckets.size && entry < buckets[bucket].size

            override fun next(): Entry {
                if (!hasNext()) throw NoSuchElementException()
                val nextBucket: Bucket = buckets[bucket]
                val nextEntry: Entry = nextBucket[entry]
                entry++
                if (entry == nextBucket.size) {
                    bucket++
                    nextBucket()
                    entry = 0
                }
                return nextEntry
            }

            private fun nextBucket() {
                while (bucket < buckets.size && buckets[bucket].isEmpty()) {
                    bucket++
                }
            }
        }
}
