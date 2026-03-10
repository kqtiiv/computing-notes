package proglang

/*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.fail

class Question5Tests {

    @Test
    fun `program 1 execution good`() {
        val store = mapOf("b" to 3, "c" to 24)
        val finalStore = program1().execute(store)
        assertEquals(
            mapOf("a" to 3, "b" to 12, "c" to 8),
            finalStore,
        )
    }

    @Test
    fun `program 1 execution bad`() {
        val store = mapOf("b" to 0, "c" to 24)
        try {
            program1().execute(store)
            fail("Expected UndefinedBehaviourException")
        } catch (_: UndefinedBehaviourException) {
            // Good: exception was expected.
        }
    }

    @Test
    fun `program 1 toString`() {
        val expected: String = """
            a = b
            b = 12
            c = c / a

        """.trimIndent()
        assertEquals(expected, program1().toString())
    }

    @Test
    fun `program 2 execution good`() {
        val store = mapOf("a" to 12, "b" to 4, "c" to 3)
        val finalStore = program2().execute(store)
        assertEquals(
            mapOf("a" to 0, "b" to 1, "c" to 3, "x" to 48, "y" to 42),
            finalStore,
        )
    }

    @Test
    fun `program 2 execution bad`() {
        val store = mapOf("a" to 12, "b" to 4, "c" to -3)
        try {
            program2().execute(store)
            fail("Expected UndefinedBehaviourException")
        } catch (_: UndefinedBehaviourException) {
            // Good: exception was expected.
        }
    }

    @Test
    fun `program 2 toString`() {
        val expected: String = """
            x = (a + b) * c
            y = x - c!
            a = 0
            b = 1 + a
            c = (2 + b)

        """.trimIndent()
        assertEquals(expected, program2().toString())
    }

    @Test
    fun `program 3 execution good 1`() {
        val store = mapOf("a" to -1)
        val finalStore = program3().execute(store)
        assertEquals(
            mapOf("a" to -1, "b" to 43),
            finalStore,
        )
    }

    @Test
    fun `program 3 execution good 2`() {
        val store = mapOf("a" to 100, "b" to 200)
        val finalStore = program3().execute(store)
        assertEquals(
            mapOf("a" to 100, "b" to 200, "c" to -42),
            finalStore,
        )
    }

    @Test
    fun `program 3 execution good 3`() {
        val store = mapOf("a" to 100, "b" to -100)
        val finalStore = program3().execute(store)
        assertEquals(
            mapOf("a" to 100, "b" to -100, "c" to 42),
            finalStore,
        )
    }

    @Test
    fun `program 3 execution bad`() {
        val store = mapOf("a" to 100)
        try {
            program3().execute(store)
            fail("Expected UndefinedBehaviourException")
        } catch (_: UndefinedBehaviourException) {
            // Good: exception was expected.
        }
    }

    @Test
    fun `program 3 toString`() {
        val expected: String = """
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
        assertEquals(expected, program3().toString())
    }

    @Test
    fun `program 4 execution good 1`() {
        val store = mapOf("a" to -1)
        val finalStore = program4().execute(store)
        assertEquals(
            mapOf("a" to -1, "b" to 43, "c" to -42, "d" to 12),
            finalStore,
        )
    }

    @Test
    fun `program 4 execution good 2`() {
        val store = mapOf("a" to 10, "b" to 5)
        val finalStore = program4().execute(store)
        assertEquals(
            mapOf("a" to 10, "b" to 5, "c" to 42, "d" to 12),
            finalStore,
        )
    }

    @Test
    fun `program 4 execution good 3`() {
        val store = mapOf("a" to 10, "b" to 20)
        val finalStore = program4().execute(store)
        assertEquals(
            mapOf("a" to 10, "b" to 20, "c" to -42, "d" to 12),
            finalStore,
        )
    }

    @Test
    fun `program 4 execution bad`() {
        val store = mapOf("b" to 0, "c" to 0, "d" to 0)
        try {
            program4().execute(store)
            fail("Expected UndefinedBehaviourException")
        } catch (_: UndefinedBehaviourException) {
            // Good: exception was expected.
        }
    }

    @Test
    fun `program 4 toString`() {
        val expected: String = """
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
        assertEquals(expected, program4().toString())
    }

    @Test
    fun `program 5 execution good 1`() {
        val store = mapOf("a" to 5, "b" to 3)
        val finalStore = program5().execute(store)
        assertEquals(
            mapOf("a" to 5, "b" to 3),
            finalStore,
        )
    }

    @Test
    fun `program 5 execution good 2`() {
        val store = mapOf("a" to 2, "b" to 4, "c" to 6, "d" to 5, "tick" to 0)
        val finalStore = program5().execute(store)
        assertEquals(
            mapOf("a" to 2, "b" to 4, "c" to 6, "d" to 5, "tick" to 1),
            finalStore,
        )
    }

    @Test
    fun `program 5 execution good 3`() {
        val store = mapOf("a" to 2, "b" to 4, "c" to 6, "d" to 8, "e" to 5, "f" to 4, "tick" to 13)
        val finalStore = program5().execute(store)
        assertEquals(
            mapOf("a" to 2, "b" to 4, "c" to 6, "d" to 8, "e" to 5, "f" to 4, "tick" to 15),
            finalStore,
        )
    }

    @Test
    fun `program 5 execution good 4`() {
        val store = mapOf("a" to 2, "b" to 4, "c" to 6, "d" to 8, "e" to 10, "f" to 20, "tick" to 22)
        val finalStore = program5().execute(store)
        assertEquals(
            mapOf("a" to 2, "b" to 4, "c" to 6, "d" to 8, "e" to 10, "f" to 20, "tick" to 25),
            finalStore,
        )
    }

    @Test
    fun `program 5 execution bad`() {
        val store = mapOf("a" to 2, "b" to 4, "c" to 6, "d" to 8, "e" to 10)
        try {
            program5().execute(store)
            fail("Expected UndefinedBehaviourException")
        } catch (_: UndefinedBehaviourException) {
            // Good: exception was expected.
        }
    }

    @Test
    fun `program 5 toString`() {
        val expected: String = """
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
        assertEquals(expected, program5().toString())
    }

    private fun program1(): SequentialProgram = SequentialProgram(
        Stmt.Assign(
            "a",
            IntExpr.Var("b"),
            Stmt.Assign(
                "b",
                IntExpr.Literal(12),
                Stmt.Assign(
                    "c",
                    IntExpr.Div(
                        IntExpr.Var("c"),
                        IntExpr.Var("a"),
                    ),
                ),
            ),
        ),
    )

    private fun program2(): SequentialProgram = SequentialProgram(
        Stmt.Assign(
            "x",
            IntExpr.Mul(
                IntExpr.Paren(
                    IntExpr.Add(
                        IntExpr.Var("a"),
                        IntExpr.Var("b"),
                    ),
                ),
                IntExpr.Var("c"),
            ),
            Stmt.Assign(
                "y",
                IntExpr.Sub(
                    IntExpr.Var("x"),
                    IntExpr.Fact(
                        IntExpr.Var("c"),
                    ),
                ),
                Stmt.Assign(
                    "a",
                    IntExpr.Literal(0),
                    Stmt.Assign(
                        "b",
                        IntExpr.Add(
                            IntExpr.Literal(1),
                            IntExpr.Var("a"),
                        ),
                        Stmt.Assign(
                            "c",
                            IntExpr.Paren(
                                IntExpr.Add(
                                    IntExpr.Literal(2),
                                    IntExpr.Var("b"),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )

    private fun program3(): SequentialProgram = SequentialProgram(
        Stmt.If(
            BoolExpr.LessThan(IntExpr.Var("a"), IntExpr.Literal(0)),
            Stmt.Assign(
                "b",
                IntExpr.Literal(42),
                Stmt.Assign("b", IntExpr.Add(IntExpr.Var("b"), IntExpr.Literal(1))),
            ),
            Stmt.If(
                BoolExpr.GreaterThan(IntExpr.Var("a"), IntExpr.Var("b")),
                Stmt.Assign("c", IntExpr.Literal(42)),
                Stmt.Assign("c", IntExpr.Sub(IntExpr.Literal(0), IntExpr.Literal(42))),
            ),
        ),
    )

    private fun program4(): SequentialProgram = SequentialProgram(
        Stmt.If(
            BoolExpr.LessThan(IntExpr.Var("a"), IntExpr.Literal(0)),
            Stmt.Assign(
                "b",
                IntExpr.Literal(42),
                Stmt.Assign(
                    "b",
                    IntExpr.Add(
                        IntExpr.Var("b"),
                        IntExpr.Literal(1),
                    ),
                ),
            ),
            null,
            Stmt.If(
                BoolExpr.GreaterThan(IntExpr.Var("a"), IntExpr.Var("b")),
                Stmt.Assign("c", IntExpr.Literal(42)),
                Stmt.Assign("c", IntExpr.Sub(IntExpr.Literal(0), IntExpr.Literal(42))),
                Stmt.Assign("d", IntExpr.Literal(12)),
            ),
        ),
    )

    private fun program5(): SequentialProgram = SequentialProgram(
        Stmt.If(
            BoolExpr.LessThan(IntExpr.Var("a"), IntExpr.Var("b")),
            Stmt.Assign(
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
            ),
        ),
    )
}
*/
