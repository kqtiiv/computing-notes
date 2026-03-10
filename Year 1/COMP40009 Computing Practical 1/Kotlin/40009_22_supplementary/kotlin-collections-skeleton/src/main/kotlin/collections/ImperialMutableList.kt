package collections

interface ImperialMutableList<T> : Iterable<T> {
    // Provides access to the number of elements in the list
    val size: Int

    // Throws an IndexOutOfBoundsException if the given index is not in the range [0, size - 1]. Otherwise, returns the
    // element at this index.
    operator fun get(index: Int): T

    // Throws an IndexOutOfBoundsException if the given index is not in the range [0, size]. Otherwise, inserts the
    // given element at this index, shifting any existing elements at or beyond this index so that they appear one
    // place later in the list. If index == size then the element is inserted at the end of the list.
    fun add(
        index: Int,
        element: T,
    )

    // Removes all elements from the list.
    fun clear()

    // Returns true if and only if the given element is contained in the list.
    fun contains(element: T): Boolean

    // Throws an IndexOutOfBoundsException if the given index is not in the range [0, size - 1]. Otherwise, returns the
    // element at this index of the list, while at the same time removing the element from the list. Existing elements
    // at larger indices of the list are shifted so that they appear one place earlier in the list.
    fun removeAt(index: Int): T

    // Searches the list for the first occurrence of the given element. If an occurrence is found, true is returned,
    // this occurrence of the element is removed from the list and elements at larger list indices are shifted so that
    // they appear one place earlier in the list. False is returned if no occurrence is found.
    fun remove(element: T): Boolean

    // Throws an IndexOutOfBoundsException if the given index is not in the range [0, size - 1]. Otherwise, sets the
    // element at this index to the given element and returns the element that was previously at this index.
    operator fun set(
        index: Int,
        element: T,
    ): T

    // Throws an IndexOutOfBoundsException if the given index is not in the range [0, size]. Otherwise, adds the
    // elements from "other" to this list, in order, starting at the given index. Any existing elements at or beyond
    // this index are moved so that they appear after the newly-inserted elements. If index == size then the elements
    // of "other" are inserted at the end of the receiving list.
    fun addAll(
        index: Int,
        other: ImperialMutableList<T>,
    )

    // Returns an iterator that provides access to the elements of the list, in order.
    override operator fun iterator(): Iterator<T>
}
