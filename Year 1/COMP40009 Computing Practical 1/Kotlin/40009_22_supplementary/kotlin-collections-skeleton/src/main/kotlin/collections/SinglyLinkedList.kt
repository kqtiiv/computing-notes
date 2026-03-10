package collections

class SinglyLinkedList<T> : ImperialMutableList<T> {
    private class Node<T>(
        var element: T,
        var next: Node<T>? = null,
    )

    private var head: Node<T>? = null

    override var size: Int = 0
        private set

    override fun toString(): String {
        val result = StringBuilder()
        result.append("[")
        var current = head
        var first = true
        while (current != null) {
            if (!first) {
                result.append(", ")
            }
            first = false
            result.append(current.element)
            current = current.next
        }
        result.append("]")
        return result.toString()
    }

    override fun get(index: Int): T {
        checkIndexInBounds(index)
        var current: Node<T>? = head
        for (i in 0..<index) {
            current = current!!.next
        }
        return current!!.element
    }

    override fun add(
        index: Int,
        element: T,
    ) {
        checkIndexInBounds(index, true)
        size++
        val (previous: Node<T>?, current: Node<T>?) = traverseTo(index)
        val newNode = Node(element, current)
        if (previous == null) {
            head = newNode
        } else {
            previous.next = newNode
        }
    }

    override fun clear() {
        head = null
        size = 0
    }

    override fun contains(element: T): Boolean {
        var current = head
        while (current != null) {
            if (current.element == element) {
                return true
            }
            current = current.next
        }
        return false
    }

    override fun removeAt(index: Int): T {
        checkIndexInBounds(index)
        val (previous: Node<T>?, current: Node<T>?) = traverseTo(index)
        val result = current!!.element
        unlink(previous, current)
        return result
    }

    override fun remove(element: T): Boolean {
        var previous: Node<T>? = null
        var current: Node<T>? = head
        while (current != null) {
            if (current.element == element) {
                unlink(previous, current)
                return true
            }
            previous = current
            current = current.next
        }
        return false
    }

    override fun set(
        index: Int,
        element: T,
    ): T {
        checkIndexInBounds(index)
        var current: Node<T>? = head
        for (i in 0..<index) {
            current = current!!.next
        }
        val result = current!!.element
        current.element = element
        return result
    }

    override fun addAll(
        index: Int,
        other: ImperialMutableList<T>,
    ) {
        checkIndexInBounds(index, true)
        val iterator = other.iterator()
        val start = Node(iterator.next())
        var end = start
        while (iterator.hasNext()) {
            end.next = Node(iterator.next())
            end = end.next!!
        }
        if (index == 0) {
            end.next = head
            head = start
        } else {
            val (previous, current) = traverseTo(index)
            assert(previous != null)
            previous!!.next = start
            end.next = current
        }
        size += other.size
    }

    override fun iterator(): Iterator<T> =
        object : Iterator<T> {
            private var nextElement: Node<T>? = head

            override fun hasNext(): Boolean = nextElement != null

            override fun next(): T {
                if (!hasNext()) {
                    throw NoSuchElementException()
                }
                val result = nextElement!!.element
                nextElement = nextElement!!.next
                return result
            }
        }

    private fun checkIndexInBounds(
        index: Int,
        inclusive: Boolean = false,
    ): Unit =
        if (index !in 0..<(if (inclusive) size + 1 else size)) {
            throw IndexOutOfBoundsException()
        } else {
            Unit
        }

    private fun unlink(
        previous: Node<T>?,
        current: Node<T>,
    ) {
        if (previous == null) {
            head = current.next
        } else {
            previous.next = current.next
        }
        size--
    }

    private fun traverseTo(index: Int): Pair<Node<T>?, Node<T>?> {
        var previous: Node<T>? = null
        var current: Node<T>? = head
        for (i in 0..<index) {
            previous = current
            current = current!!.next
        }
        return Pair(previous, current)
    }
}
