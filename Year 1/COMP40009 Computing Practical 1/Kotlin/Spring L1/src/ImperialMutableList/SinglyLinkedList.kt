package collections

import ImperialMutableList.ImperialMutableList

// this includes a package that includes the class Node
// this is bad, as compiler will get confused!

// General rule: wherever possible,
// hide internal details of your classes

// Empty primary constructor – better style to omit it
class SinglyLinkedList<T> : ImperialMutableList<T> {
    private class Node<T>(
        var element: T,
        var next: Node<T>? = null,
    )

    private var head: Node<T>? = null
    private var tail: Node<T>? = null
    override var size: Int = 0
        private set

    override fun add(element: T) {
        size++
        val newNode = Node(element, null)
        if (head == null) {
            head = newNode
            tail = newNode
            return
        }
        tail!!.next = newNode
        // we know that it is not null, as only way this is null, is if it is empty
        // we will break out the function if it is empty in the if statment
        tail = newNode
    }

    override fun add(
        index: Int,
        element: T,
    ) {
        if (index !in 0..size) {
            throw IndexOutOfBoundsException()
        }

        if (index == 0) {
            val newNode = Node(element, head)
            head = newNode
            if (size == 0) {
                tail = newNode
            }
            size++
            return
        }

        val (prev, cur) = traverseTo(index)
        val newNode = Node(element, cur)
        prev!!.next = newNode

        if (index == size) {
            tail = newNode
        }
        size++
    }

    override fun removeAt(index: Int): T {
        if (index !in 0..<size) {
            throw IndexOutOfBoundsException()
        }
        val (previous: Node<T>?, current: Node<T>) = traverseTo(index)
        unlink(previous, current)
        return current.element
    }

    override fun remove(element: T): Boolean {
        var previous: Node<T>? = null
        var current = head

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

    override fun clear() {
        head = null
        tail = null
        size = 0
    }

    override fun get(index: Int): T {
        if (index !in 0..<size) {
            throw IndexOutOfBoundsException()
        }
        val (_, current: Node<T>) = traverseTo(index)
        return current.element
    }

    override fun contains(element: T): Boolean {
        var current = head
        while (current != null) {
            if (current.element == element) return true
            current = current.next
        }
        return false
    }

    override fun set(
        index: Int,
        element: T,
    ) {
        if (index !in 0..size) throw IndexOutOfBoundsException()
        var curNode = head
        for (i in 0..<index) {
            curNode = curNode!!.next
        }
        curNode!!.element = element
    }

    override fun iterator(): Iterator<T> =
        object : Iterator<T> {
            private var nextNode: Node<T>? = head

            override fun hasNext(): Boolean = nextNode != null

            override fun next(): T =
                if (!this.hasNext()) {
                    throw NoSuchElementException()
                } else {
                    val result = nextNode!!.element
                    nextNode = nextNode!!.next
                    result
                }
        }

    private fun traverseTo(index: Int): Pair<Node<T>?, Node<T>> {
        var previous: Node<T>? = null
        var current: Node<T>? = head
        for (i in 0..<index) {
            previous = current
            current = current!!.next
        }
        return Pair(previous, current!!)
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
        if (current == tail) {
            tail = previous
        }
        size--
    }
}

fun <T> combine(
    first: ImperialMutableList<T>,
    second: ImperialMutableList<T>,
): ImperialMutableList<T> {
    val result = SinglyLinkedList<T>()
    for (element in first) {
        result.add(element)
    }
    for (element in second) {
        result.add(element)
    }
    return result
}

fun main() {
    val myLinkedList = SinglyLinkedList<Int>()
}
