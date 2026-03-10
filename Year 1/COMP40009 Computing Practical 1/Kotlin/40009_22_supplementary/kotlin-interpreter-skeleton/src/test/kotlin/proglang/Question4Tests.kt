package proglang

/*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlin.test.assertSame

class Question4Tests {

    @Test
    fun `step stmt1`() {
        val store = mutableMapOf("b" to 3, "c" to 24)
        val stmts = stmt1()
        val continuation = stmts[0].step(store)
        assertSame(stmts[1], continuation)
        assertEquals(mapOf("a" to 3, "b" to 3, "c" to 24), store)
    }

    @Test
    fun `step stmt2`() {
        val store = mutableMapOf("a" to 12, "b" to 4, "c" to 3)
        val stmts = stmt2()
        val continuation = stmts[0].step(store)
        assertSame(stmts[1], continuation)
        assertEquals(mapOf("x" to 48, "a" to 12, "b" to 4, "c" to 3), store)
    }

    @Test
    fun `step stmt3`() {
        val store = mutableMapOf("a" to -1)
        val stmts = stmt3()
        val continuation = stmts.first[0].step(store)
        assertSame(stmts.second, continuation)
        assertEquals(mapOf("a" to -1), store)
    }

    @Test
    fun `step stmt4 true condition`() {
        val store = mutableMapOf("a" to -1)
        val stmts = stmt4()
        val continuation = stmts.first[0].step(store)
        assertSame(stmts.second, continuation)
        assertEquals(mapOf("a" to -1), store)
    }

    @Test
    fun `step stmt4 false condition`() {
        val store = mutableMapOf("a" to 1)
        val stmts = stmt4()
        val continuation = stmts.first[0].step(store)
        assertSame(stmts.first[1], continuation)
        assertEquals(mapOf("a" to 1), store)
    }

    @Test
    fun `step stmt5 true condition`() {
        val store = mutableMapOf("a" to 3, "b" to 5)
        val stmts = stmt5()
        val continuation = stmts.first[0].step(store)
        assertSame(stmts.second, continuation)
        assertEquals(mapOf("a" to 3, "b" to 5), store)
    }

    @Test
    fun `step stmt5 false condition`() {
        val store = mutableMapOf("a" to 5, "b" to 3)
        val stmts = stmt5()
        val continuation = stmts.first[0].step(store)
        assertNull(continuation)
        assertEquals(mapOf("a" to 5, "b" to 3), store)
    }
}

private fun stmt1(): List<Stmt> {
    val stmt1c = Stmt.Assign("c", IntExpr.Div(IntExpr.Var("c"), IntExpr.Var("a")))

    val stmt1b = Stmt.Assign(
        "b",
        IntExpr.Literal(12),
        stmt1c,
    )

    val stmt1a: Stmt = Stmt.Assign("a", IntExpr.Var("b"), stmt1b)

    return listOf(stmt1a, stmt1b, stmt1c)
}

private fun stmt2(): List<Stmt> {
    val stmt2e = Stmt.Assign("c", IntExpr.Paren(IntExpr.Add(IntExpr.Literal(2), IntExpr.Var("b"))))

    val stmt2d = Stmt.Assign(
        "b",
        IntExpr.Add(IntExpr.Literal(1), IntExpr.Var("a")),
        stmt2e,
    )
    val stmt2c = Stmt.Assign(
        "a",
        IntExpr.Literal(0),
        stmt2d,
    )
    val stmt2b = Stmt.Assign(
        "y",
        IntExpr.Sub(IntExpr.Var("x"), IntExpr.Fact(IntExpr.Var("c"))),
        stmt2c,
    )
    val stmt2a = Stmt.Assign(
        "x",
        IntExpr.Mul(IntExpr.Paren(IntExpr.Add(IntExpr.Var("a"), IntExpr.Var("b"))), IntExpr.Var("c")),
        stmt2b,
    )
    return listOf(stmt2a, stmt2b, stmt2c, stmt2d, stmt2e)
}

private fun stmt3(): Triple<List<Stmt>, Stmt, Stmt> {
    val elseStmt = Stmt.If(
        BoolExpr.GreaterThan(IntExpr.Var("a"), IntExpr.Var("b")),
        Stmt.Assign("c", IntExpr.Literal(42)),
        Stmt.Assign("c", IntExpr.Sub(IntExpr.Literal(0), IntExpr.Literal(42))),
    )
    val thenStmt = Stmt.Assign(
        "b",
        IntExpr.Literal(42),
        Stmt.Assign("b", IntExpr.Add(IntExpr.Var("b"), IntExpr.Literal(1))),
    )
    val stmt3a: Stmt = Stmt.If(
        BoolExpr.LessThan(IntExpr.Var("a"), IntExpr.Literal(0)),
        thenStmt,
        elseStmt,
    )
    return Triple(listOf(stmt3a), thenStmt, elseStmt)
}

private fun stmt4(): Pair<List<Stmt>, Stmt> {
    val stmt4c = Stmt.Assign("d", IntExpr.Literal(12))

    val stmt4b = Stmt.If(
        BoolExpr.GreaterThan(IntExpr.Var("a"), IntExpr.Var("b")),
        Stmt.Assign("c", IntExpr.Literal(42)),
        Stmt.Assign("c", IntExpr.Sub(IntExpr.Literal(0), IntExpr.Literal(42))),
        stmt4c,
    )

    val thenStmt = Stmt.Assign(
        "b",
        IntExpr.Literal(42),
        Stmt.Assign(
            "b",
            IntExpr.Add(
                IntExpr.Var("b"),
                IntExpr.Literal(1),
            ),
        ),
    )
    val stmt4a: Stmt = Stmt.If(
        BoolExpr.LessThan(IntExpr.Var("a"), IntExpr.Literal(0)),
        thenStmt,
        null,
        stmt4b,
    )

    return Pair(listOf(stmt4a, stmt4b, stmt4c), thenStmt)
}

private fun stmt5(): Pair<List<Stmt>, Stmt> {
    val thenStmt = Stmt.Assign(
        "tick",
        IntExpr.Add(IntExpr.Var("tick"), IntExpr.Literal(1)),
        Stmt.If(
            BoolExpr.LessThan(IntExpr.Var("c"), IntExpr.Var("d")),
            Stmt.Assign(
                "tick",
                IntExpr.Add(IntExpr.Var("tick"), IntExpr.Literal(1)),
                Stmt.If(
                    BoolExpr.LessThan(IntExpr.Var("e"), IntExpr.Var("f")),
                    Stmt.Assign("tick", IntExpr.Add(IntExpr.Var("tick"), IntExpr.Literal(1))),
                    Stmt.If(
                        BoolExpr.GreaterThan(IntExpr.Var("a"), IntExpr.Var("b")),
                        Stmt.Assign("tick", IntExpr.Var("unreachable")),
                    ),
                ),
            ),
            Stmt.If(
                BoolExpr.LessThan(IntExpr.Var("c"), IntExpr.Var("d")),
                Stmt.Assign("tick", IntExpr.Div(IntExpr.Literal(0), IntExpr.Literal(0))),
            ),
        ),
    )
    val stmt5a: Stmt = Stmt.If(
        BoolExpr.LessThan(IntExpr.Var("a"), IntExpr.Var("b")),
        thenStmt,
    )
    return Pair(listOf(stmt5a), thenStmt)
}
*/
