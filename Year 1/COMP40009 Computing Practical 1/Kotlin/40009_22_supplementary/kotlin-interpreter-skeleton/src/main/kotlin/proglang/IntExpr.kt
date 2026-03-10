package proglang

interface IntExpr {
    class Add(
        val lhs: IntExpr,
        val rhs: IntExpr,
    ) : IntExpr
}

fun IntExpr.eval(store: Map<String, Int>): Int =
    when (this) {
        is IntExpr.Add -> lhs.eval(store) + rhs.eval(store)
        else -> throw UnsupportedOperationException("The above should account for all kinds of IntExpr.")
    }
