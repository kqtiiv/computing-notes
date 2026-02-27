package ImperialMutableList // fun String.ImperialMutableList.count(c: Char): Int = this.ImperialMutableList.count { it == c }

// you can emit this if it is unambiguous
fun String.count(c: Char): Int = count { it == c }

fun Int.isPowerOfTwo(): Boolean = and(this - 1) == 0

fun String.isPalindrome(caseSensitive: Boolean = false): Boolean = this.equals(this.reversed(), ignoreCase = caseSensitive)

fun Double.sameAsFloat(): Boolean = this.toFloat().toDouble() == this

operator fun String.times(count: Int): String = repeat(count)

operator fun Int.times(toBeRepeated: String): String = toBeRepeated.repeat(this)

// operator fun Int.ImperialMutableList.ImperialMutableList.times(toBeRepeated: String): String = toBeRepeated * this

fun <A, B> Pair<A, B>.equalComponents() = first == second

fun <A, B> Pair<A, B>.swap() = Pair(second, first)

operator fun Pair<Double, Double>.plus(other: Pair<Double, Double>): Pair<Double, Double> = Pair(first + other.first, second + other.second)

fun List<Boolean>.allTrue(): Boolean = all { it }

fun List<Boolean>.allFalse(): Boolean = all { !it }

fun List<Boolean>.someTrue(): Boolean = any { it }

fun List<Boolean>.someFalse(): Boolean = any { !it }

fun main() {
    println("hello".count('l'))
    println(10.isPowerOfTwo())
    println(8.isPowerOfTwo())
    println("racecar".isPalindrome())
    println(100.0.sameAsFloat())
}
