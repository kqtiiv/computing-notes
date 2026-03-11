package proglang

sealed interface IntExpr {
    class Add(
        val lhs: IntExpr,
        val rhs: IntExpr,
    ) : IntExpr {
        override fun toString(): String = "$lhs + $rhs"
    }

    class Literal(
        val value: Int,
    ) : IntExpr {
        override fun toString(): String = "$value"
    }

    class Var(
        val name: String,
    ) : IntExpr {
        override fun toString(): String = name
    }

    class Mul(
        val lhs: IntExpr,
        val rhs: IntExpr,
    ) : IntExpr {
        override fun toString(): String = "$lhs * $rhs"
    }

    class Sub(
        val lhs: IntExpr,
        val rhs: IntExpr,
    ) : IntExpr {
        override fun toString(): String = "$lhs - $rhs"
    }

    class Div(
        val lhs: IntExpr,
        val rhs: IntExpr,
    ) : IntExpr {
        override fun toString(): String = "$lhs / $rhs"
    }

    class Fact(
        val expr: IntExpr,
    ) : IntExpr {
        override fun toString(): String = "$expr!"
    }

    class Paren(
        val expr: IntExpr,
    ) : IntExpr {
        override fun toString(): String = "($expr)"
    }
}

fun IntExpr.eval(store: Map<String, Int>): Int =
    when (this) {
        is IntExpr.Add -> {
            lhs.eval(store) + rhs.eval(store)
        }

        is IntExpr.Literal -> {
            value
        }

        is IntExpr.Var -> {
            store[name] ?: throw UndefinedBehaviourException("The variable does not provide a value")
        }

        is IntExpr.Mul -> {
            lhs.eval(store) * rhs.eval(store)
        }

        is IntExpr.Sub -> {
            lhs.eval(store) - rhs.eval(store)
        }

        is IntExpr.Div -> {
            val rhsResult = rhs.eval(store)
            if (rhsResult == 0) {
                throw UndefinedBehaviourException("The right-hand-side evaluates to zero")
            } else {
                lhs.eval(store) / rhsResult
            }
        }

        is IntExpr.Fact -> {
            val result = expr.eval(store)

            tailrec fun evalFact(
                n: Int,
                acc: Int = 1,
            ): Int {
                if (n < 0) throw UndefinedBehaviourException("Cannot apply factorial on a negative integer")
                if (n == 0) return acc
                return evalFact(n - 1, n * acc)
            }

            evalFact(result)
        }

        is IntExpr.Paren -> {
            expr.eval(store)
        }
    }
