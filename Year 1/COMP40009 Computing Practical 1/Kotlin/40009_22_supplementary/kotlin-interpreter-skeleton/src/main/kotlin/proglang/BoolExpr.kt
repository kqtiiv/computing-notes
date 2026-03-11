package proglang

sealed interface BoolExpr {
    class LessThan(
        val lhs: IntExpr,
        val rhs: IntExpr,
    ) : BoolExpr {
        override fun toString(): String = "$lhs < $rhs"
    }

    class GreaterThan(
        val lhs: IntExpr,
        val rhs: IntExpr,
    ) : BoolExpr {
        override fun toString(): String = "$lhs > $rhs"
    }

    class Equals(
        val lhs: IntExpr,
        val rhs: IntExpr,
    ) : BoolExpr {
        override fun toString(): String = "$lhs == $rhs"
    }

    class And(
        val lhs: BoolExpr,
        val rhs: BoolExpr,
    ) : BoolExpr {
        override fun toString(): String = "$lhs && $rhs"
    }

    class Or(
        val lhs: BoolExpr,
        val rhs: BoolExpr,
    ) : BoolExpr {
        override fun toString(): String = "$lhs || $rhs"
    }

    class Not(
        val expr: BoolExpr,
    ) : BoolExpr {
        override fun toString(): String = "!$expr"
    }

    class Paren(
        val expr: BoolExpr,
    ) : BoolExpr {
        override fun toString(): String = "($expr)"
    }
}

fun BoolExpr.eval(store: Map<String, Int>): Boolean =
    when (this) {
        is BoolExpr.Paren -> expr.eval(store)
        is BoolExpr.Not -> !expr.eval(store)
        is BoolExpr.And -> lhs.eval(store) && rhs.eval(store)
        is BoolExpr.Or -> lhs.eval(store) || rhs.eval(store)
        is BoolExpr.Equals -> lhs.eval(store) == rhs.eval(store)
        is BoolExpr.LessThan -> lhs.eval(store) < rhs.eval(store)
        is BoolExpr.GreaterThan -> lhs.eval(store) > rhs.eval(store)
    }
