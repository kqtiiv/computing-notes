package ImperialMutableList

// fixed capacity list: not very useful
// if capacity too small, RUN OUT OF SPACE
// if too large, WASTE MEMORY

// array lists: starts with an array of some initial capacity
// when array is full, switch to new array with double the capacity

private val DEFAULT_INITIAL_CAPACITY: Int = 16

class ResizingArrayList<T>(
    private val initialCapacity: Int,
) : ImperialMutableList<T> {
    init {
        if (initialCapacity < 0) {
            throw IllegalArgumentException()
        }
    }

    override var size = 0
        private set

    private var elements: Array<T?> = clearedArray()
    constructor() : this(DEFAULT_INITIAL_CAPACITY)

    private fun clearedArray(): Array<T?> = arrayOfNulls(initialCapacity)

    // inner class not good, as we dont want any other methods to reference it
//    private inner class ResizingArrayListIterator : Iterator<T> {
//        private var nextIndex: Int = 0
//
//        override fun hasNext(): Boolean = nextIndex < size
//
//        override fun next(): T =
//            if (!hasNext()) {
//                throw NoSuchElementException()
//            } else {
//                this@ImperialMutableList.ResizingArrayList[nextIndex++]
//            }
//    }

    // the cost of resizeing a list is amortized
    // resizes are rare and becomes RARER as size of array is doubled
    override fun add(
        index: Int,
        element: T,
    ) {
        require(index !in 0..size)
//        if (index !in 0..size) {
//            throw IndexOutOfBoundsException()
//        }
        if (size++ == elements.size) {
            val newArray: Array<T?> = arrayOfNulls(2 * elements.size)
            for (i in 0..<size) {
                newArray[i] = elements[i]
            }
            elements = newArray
        }
        for (i in size downTo index + 1) {
            elements[i] = elements[i - 1]
        }
        elements[index] = element
    }

    override fun add(element: T) = this.add(size++, element)

    override fun contains(element: T): Boolean = element in elements

    override fun get(index: Int): T {
        require(index in 0..<size)
        return elements[index]!!
    }

    override fun remove(element: T): Boolean {
        require(size > 0)
        for (i in 0 until size) {
            if (elements[i] == element) {
                removeAt(i)
                return true
            }
        }
        return false
    }

    override fun removeAt(index: Int): T {
        require(index in 0 until size)
        val element = elements[index]
        for (i in size - 1 downTo index) {
            elements[i] = elements[i + 1]
        }
        size--
        return element!!
    }

    override fun set(
        index: Int,
        element: T,
    ) {
        elements[index] = element
    }

    override fun clear() {
        size = 0
        elements = clearedArray()
    }

//    override fun iterator(): Iterator<T> = ResizingArrayListIterator()
    override fun iterator(): Iterator<T> =
        object : Iterator<T> {
            private var nextIndex = 0

            override fun hasNext(): Boolean = nextIndex < size

            override fun next(): T = elements[nextIndex++]!!
        }
}
