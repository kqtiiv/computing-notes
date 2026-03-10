package collections

fun <S : T, T> ImperialMutableList<T>.removeAll(other: ImperialMutableList<S>) {
    for (elem in other) {
        remove(elem)
    }
}
