package ImperialMutableList

class FixedCapacityList<T>(
    capacity: Int,
) : ImperialMutableList<T> {
    override var size = 0
        private set

    // downcasting for any fcl - this is because uppercase() only works on Strings
//    val upperCaseMinty = (myStrings.get(0) as String)
//        .uppercase()

    // you can set the default value to 0, as ints are of type nullable Any
    private val elements: Array<T?> =
        if (capacity < 0) {
            throw IllegalArgumentException("capacity must be greater than 0")
        } else {
            arrayOfNulls(capacity)
        }

    override fun toString(): String =
        elements
            .slice(0..<size)
            .joinToString(prefix = "[", postfix = "]")

    override fun add(
        index: Int,
        element: T,
    ) {
        require(index !in 0..size || size == elements.size)

        for (i in size downTo index + 1) {
            elements[i] = elements[i - 1]
        }
        elements[index] = element
        size++
    }

    //  overloading (add hoc polymorphism)
    override fun add(element: T) = add(size, element)

    override fun iterator(): Iterator<T> {
        TODO("Not yet implemented")
    }

    override fun get(index: Int): T =
        if (index !in 0..<size) {
            throw IllegalArgumentException("index out of range")
        } else {
            elements[index]!! // non-null assertion! we know that this will NOT be null
        }

    // with the list of anys, we should reset the elements of the list to a default value
    // as any can be a lot of storage, e.g., a 100gb video
    override fun clear() {
        for (i in 0 until size) {
            elements[i] = null
        }
        size = 0
    }

    override fun contains(element: T): Boolean = element in elements

    override fun removeAt(index: Int): T =
        if (index !in 0..<size) {
            throw IndexOutOfBoundsException()
        } else {
            val removedElement = elements[index]
            for (i in index until size - 1) {
                elements[i] = elements[i + 1]
            }
            size--
            removedElement!!
        }

    override fun remove(x: T): Boolean {
        for (i in 0 until size) {
            if (elements[i] == x) {
                removeAt(i)
                return true
            }
        }
        return false
    }

    override fun set(
        index: Int,
        element: T,
    ) = if (index !in 0..<size) {
        throw IllegalArgumentException("index out of range")
    } else {
        elements[index] = element
    }
}

fun main() {
}
