package proglang

/*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlin.test.assertSame

class Question3Tests {

    @Test
    fun `next stmt1`() {
        val stmts = stmt1()
        assertSame(stmts[1], stmts[0].next)
        assertSame(stmts[2], stmts[0].next!!.next)
        assertNull(stmts[0].next!!.next!!.next)
    }

    @Test
    fun `next stmt2`() {
        val stmts = stmt2()
        assertSame(stmts[1], stmts[0].next)
        assertSame(stmts[2], stmts[0].next!!.next)
        assertSame(stmts[3], stmts[0].next!!.next!!.next)
        assertSame(stmts[4], stmts[0].next!!.next!!.next!!.next)
        assertNull(stmts[0].next!!.next!!.next!!.next!!.next)
    }

    @Test
    fun `next stmt3`() {
        val stmts = stmt3()
        assertNull(stmts[0].next)
    }

    @Test
    fun `next stmt4`() {
        val stmts = stmt4()
        assertSame(stmts[1], stmts[0].next)
        assertSame(stmts[2], stmts[0].next!!.next)
        assertNull(stmts[0].next!!.next!!.next)
    }

    @Test
    fun `next stmt5`() {
        val stmts = stmt5()
        assertNull(stmts[0].next)
    }

    @Test
    fun `lastInSequence stmt1`() {
        val stmts = stmt1()
        assertSame(stmts.last(), stmts[0].lastInSequence)
    }

    @Test
    fun `lastInSequence stmt2`() {
        val stmts = stmt2()
        assertSame(stmts.last(), stmts[0].lastInSequence)
    }

    @Test
    fun `lastInSequence stmt3`() {
        val stmts = stmt3()
        assertSame(stmts.last(), stmts[0].lastInSequence)
    }

    @Test
    fun `lastInSequence stmt4`() {
        val stmts = stmt4()
        assertSame(stmts.last(), stmts[0].lastInSequence)
    }

    @Test
    fun `lastInSequence stmt5`() {
        val stmts = stmt5()
        assertSame(stmts.last(), stmts[0].lastInSequence)
    }

    @Test
    fun `toString stmt1`() {
        val expected = """
            a = b
            b = 12
            c = c / a

        """.trimIndent()
        assertEquals(expected, stmt1()[0].toString())
    }

    @Test
    fun `toString stmt2`() {
        val expected = """
            x = (a + b) * c
            y = x - c!
            a = 0
            b = 1 + a
            c = (2 + b)

        """.trimIndent()
        assertEquals(expected, stmt2()[0].toString())
    }

    @Test
    fun `toString stmt3`() {
        val expected = """
            if (a < 0) {
                b = 42
                b = b + 1
            } else {
                if (a > b) {
                    c = 42
                } else {
                    c = 0 - 42
                }
            }

        """.trimIndent()
        assertEquals(expected, stmt3()[0].toString())
    }

    @Test
    fun `toString stmt4`() {
        val expected = """
            if (a < 0) {
                b = 42
                b = b + 1
            }
            if (a > b) {
                c = 42
            } else {
                c = 0 - 42
            }
            d = 12

        """.trimIndent()
        assertEquals(expected, stmt4()[0].toString())
    }

    @Test
    fun `toString stmt5`() {
        val expected = """
            if (a < b) {
                tick = tick + 1
                if (c < d) {
                    tick = tick + 1
                    if (e < f) {
                        tick = tick + 1
                    } else {
                        if (a > b) {
                            tick = unreachable
                        }
                    }
                } else {
                    if (c < d) {
                        tick = 0 / 0
                    }
                }
            }

        """.trimIndent()
        assertEquals(expected, stmt5()[0].toString())
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

private fun stmt3(): List<Stmt> {
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
    return listOf(stmt3a)
}

private fun stmt4(): List<Stmt> {
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

    return listOf(stmt4a, stmt4b, stmt4c)
}

private fun stmt5(): List<Stmt> {
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
    return listOf(stmt5a)
}
*/
