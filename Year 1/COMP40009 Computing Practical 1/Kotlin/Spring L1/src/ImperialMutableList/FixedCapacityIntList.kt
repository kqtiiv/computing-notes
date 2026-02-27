package ImperialMutableList

// TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or
// click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
class FixedCapacityIntList(
    capacity: Int,
) {
    // make it private, so the user cannot update the size
    // BUT has public READ property
    var size: Int = 0
        private set

    override fun toString(): String =
        elements
            .slice(0..<size) // add < to make sure it is non-inclusive
            .joinToString(prefix = "[", postfix = "]") // good practice to add [] to show it's a list

//    override fun toString(): String =
//        elements
//            .contentToString() // this function only works on arrays

    // make the array val, as it refers to the same array in memory each time
    // with var, it is possible to change the reference to the array to a completely different
    // location in memory
    // val still allows each INDIVIDUAL element in the array point to a different value, so is still mutable
    private val elements: Array<Int> =
        if (capacity < 0) {
            throw IllegalArgumentException("capacity must be greater than 0")
        } else {
            Array(capacity) { -1 }
        }

    fun add(
        index: Int,
        element: Int,
    ) {
        // do not increment the size of the list/add if index is out of bounds
        if (index !in 0..size) throw IllegalArgumentException("index out of range")
        // camnot use capacity, as it is out of scope
        // also do not make it a attribute of the class, as it is redundant
        // just reference elements.size
        if (size == elements.size) throw IllegalArgumentException("list is full")
        for (i in size downTo index + 1) {
            elements[i] = elements[i - 1]
        }
        elements[index] = element
        size++
    }

    //  overloading (add hoc polymorphism)
    fun add(element: Int) = add(size, element)

    fun get(index: Int) =
        if (index !in 0..<size) {
            throw IllegalArgumentException("index out of range")
        } else {
            elements[index]
        }

    fun clear() {
        size = 0
    }

    fun contains(element: Int) = element in elements

    fun removeAt(index: Int): Boolean =
        if (index !in 0..<size) {
            throw IndexOutOfBoundsException()
        } else {
            for (i in index until size - 1) {
                elements[i] = elements[i + 1]
            }
            size--
            true
        }

    fun remove(x: Int): Boolean {
        for (i in 0 until size) {
            if (elements[i] == x) {
                removeAt(i)
                return true
            }
        }
        return false
    }

    fun set(
        index: Int,
        element: Int,
    ) = if (index !in 0..<size) {
        throw IllegalArgumentException("index out of range")
    } else {
        elements[index] = element
    }
}

fun main() {
    try {
        val fcl = FixedCapacityIntList(10)
        fcl.add(1)
        fcl.add(2)
        fcl.add(3)
        fcl.add(4)
        fcl.add(5)
        fcl.add(6)
        fcl.removeAt(2)
        println("size: ${fcl.size}")
        println("elements: $fcl")
    } catch (exception: Exception) {
        // not good practice to catch all
        exception.printStackTrace()
    }
}

// psvm to make a ImperialMutableList.main() function
