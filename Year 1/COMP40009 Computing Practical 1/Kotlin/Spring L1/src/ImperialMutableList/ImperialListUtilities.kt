package ImperialMutableList

import collections.SinglyLinkedList

fun <T, U> ImperialMutableList<T>.map(transform: (T) -> U): ImperialMutableList<U> {
    val result = SinglyLinkedList<U>()
    for (element in this) result.add(transform(element))
    return result
}

fun <T> ImperialMutableList<T>.filter(function: (T) -> Boolean): ImperialMutableList<T> {
    val result = SinglyLinkedList<T>()
    for (element in this) {
        if (function(element)) result.add(element)
    }
    return result
}

fun <T, U> ImperialMutableList<T>.zip(otherList: ImperialMutableList<U>): ImperialMutableList<Pair<T, U>> {
    val result = SinglyLinkedList<Pair<T, U>>()
    val iter1 = this.iterator()
    val iter2 = otherList.iterator()
    while (iter1.hasNext() && iter2.hasNext()) {
        result.add(Pair(iter1.next(), iter2.next()))
    }
    return result
}

fun <T> ImperialMutableList<T>.reduce(function: (T, T) -> T): T {
    if (this.isEmpty()) throw IllegalArgumentException()
    val iter = this.iterator()
    var result = iter.next()
    while (iter.hasNext()) {
        result = function(result, iter.next())
    }
    return result
}

fun <T> ImperialMutableList<T>.reduce(
    function: (T, T) -> T,
    init: T,
): T {
    if (this.isEmpty()) throw IllegalArgumentException()
    val iter = this.iterator()
    var result = init
    while (iter.hasNext()) {
        result = function(result, iter.next())
    }
    return result
}
