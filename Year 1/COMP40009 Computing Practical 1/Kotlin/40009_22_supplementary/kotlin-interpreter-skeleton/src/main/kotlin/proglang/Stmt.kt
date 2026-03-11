package proglang

const val INDENT_SPACES = 4
const val INITIAL_INDENT = 0

sealed interface Stmt {
    var next: Stmt?
    val lastInSequence: Stmt
        get() = next?.lastInSequence ?: this

    fun toString(indent: Int): String

    fun clone(): Stmt

    class Assign(
        val name: String,
        val expr: IntExpr,
        override var next: Stmt? = null,
    ) : Stmt {
        override fun toString(indent: Int): String =
            buildString {
                append(" ".repeat(indent))
                append("$name = $expr\n")
                next?.let { append(it.toString(indent)) }
            }

        override fun toString(): String = toString(INITIAL_INDENT)

        override fun clone(): Stmt = Assign(name, expr, next?.clone())
    }

    class If(
        val condition: BoolExpr,
        val thenStmt: Stmt,
        val elseStmt: Stmt? = null,
        override var next: Stmt? = null,
    ) : Stmt {
        override fun toString(indent: Int): String =
            buildString {
                val indented: String = " ".repeat(indent)
                append(indented)
                append("if ($condition) {\n")
                append(thenStmt.toString(indent + INDENT_SPACES))
                if (elseStmt != null) {
                    append(indented)
                    append("} else {\n")
                    append(elseStmt.toString(indent + INDENT_SPACES))
                }
                append(indented)
                append("}\n")
                next?.let { append(it.toString(indent)) }
            }

        override fun toString(): String = toString(INITIAL_INDENT)

        override fun clone(): Stmt = If(condition, thenStmt.clone(), elseStmt?.clone(), next?.clone())
    }

    class While(
        val condition: BoolExpr,
        val body: Stmt?,
        override var next: Stmt? = null,
    ) : Stmt {
        override fun toString(indent: Int): String =
            buildString {
                val indented: String = " ".repeat(indent)
                append(indented)
                append("while ($condition) {\n")
                if (body != null) {
                    append(body.toString(indent + INDENT_SPACES))
                }
                append(indented)
                append("}\n")
                next?.let { append(it.toString(indent)) }
            }

        override fun toString(): String = toString(INITIAL_INDENT)

        override fun clone(): Stmt = While(condition, body?.clone(), next?.clone())
    }
}

fun Stmt.step(store: MutableMap<String, Int>): Stmt? =
    when (this) {
        is Stmt.Assign -> {
            store[name] = expr.eval(store)
            next
        }

        is Stmt.If -> {
            if (condition.eval(store)) {
                thenStmt.lastInSequence.next = next
                thenStmt
            } else {
                elseStmt?.lastInSequence?.next = next
                elseStmt ?: next
            }
        }

        is Stmt.While -> {
            if (condition.eval(store)) {
                if (body != null) {
                    val loop = body.clone()
                    loop.lastInSequence.next = this
                    loop
                } else {
                    this
                }
            } else {
                next
            }
        }
    }
