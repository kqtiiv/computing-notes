package ImperialMutableList

data class Point(
    val first: Int,
    val second: Int,
) {
    infix operator fun plus(other: Point): Point =
        Point(
            this.first + other.first,
            this.second + other.second,
        )
}

operator fun Int.times(point: Point): Point =
    Point(
        this * point.first,
        this * point.second,
    )
