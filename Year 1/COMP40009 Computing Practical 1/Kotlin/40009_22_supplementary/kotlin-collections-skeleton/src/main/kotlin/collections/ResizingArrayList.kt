package collections

import kotlin.math.max
import kotlin.math.min

private const val DEFAULT_CAPACITY = 16

class ResizingArrayList<T>(
    capacity: Int = DEFAULT_CAPACITY,
) : ImperialMutableList<T> {
    private var elements: Array<T?> = arrayOfNulls<Any?>(capacity) as Array<T?>

    init {
        require(capacity >= 0)
    }

    override var size: Int = 0
        private set

    override fun add(
        index: Int,
        element: T,
    ) {
        checkIndexInBounds(index, true)
        if (size >= elements.size) {
            resize(max(size + 1, 2 * size))
        }
        for (i in size downTo index + 1) {
            elements[i] = elements[i - 1]
        }
        elements[index] = element
        size++
    }

    override fun addAll(
        index: Int,
        other: ImperialMutableList<T>,
    ) {
        checkIndexInBounds(index, true)
        if (size + other.size > elements.size) {
            resize(max(size + other.size, 2 * size))
        }
        for (i in size - 1 downTo index) {
            elements[i + other.size] = elements[i]
        }
        for (i in 0 until other.size) {
            elements[index + i] = other[i]
        }
        size += other.size
    }

    override fun set(
        index: Int,
        element: T,
    ): T {
        checkIndexInBounds(index)
        val result = elements[index]!!
        elements[index] = element
        return result
    }

    override fun clear() {
        for (i in 0 until size) {
            elements[i] = null
        }
        size = 0
    }

    override fun contains(element: T): Boolean = elements.contains(element)

    override fun get(index: Int): T {
        checkIndexInBounds(index)
        return elements[index]!!
    }

    override fun iterator(): Iterator<T> =
        object : Iterator<T> {
            private var curIndex: Int = 0

            override fun hasNext(): Boolean = curIndex < size

            override fun next(): T {
                if (!hasNext()) {
                    throw NoSuchElementException()
                }
                return elements[curIndex++]!!
            }
        }

    override fun remove(element: T): Boolean {
        for (i in 0 until size) {
            if (elements[i] == element) {
                removeAt(i)
                return true
            }
        }
        return false
    }

    override fun removeAt(index: Int): T {
        checkIndexInBounds(index)
        size--
        val removed = elements[index]!!
        for (i in index..<size) {
            elements[i] = elements[i + 1]
        }
        elements[size] = null

        return removed
    }

    override fun toString(): String =
        elements
            .slice(0..<size)
            .joinToString(", ", "[", "]")

    private fun checkIndexInBounds(
        index: Int,
        inclusive: Boolean = false,
    ): Unit =
        if (index !in 0..<(if (inclusive) size + 1 else size)) {
            throw IndexOutOfBoundsException()
        } else {
            Unit
        }

    private fun resize(newSize: Int) {
        elements = elements.copyOf(newSize)
    }
}
