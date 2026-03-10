package proglang

/*
import kotlin.test.Test
import kotlin.test.assertEquals

class Question6Tests {

    @Test
    fun `loopy program 1 execution 1`() {
        val finalStore = loopyProgram1().execute(
            mapOf("value" to 10, "repeats" to 20),
        )
        assertEquals(mapOf("value" to 10, "repeats" to 20, "counter" to 20, "result" to 200), finalStore)
    }

    @Test
    fun `loopy program 1 execution 2`() {
        val finalStore = loopyProgram1().execute(
            mapOf("value" to 1, "repeats" to 5),
        )
        assertEquals(mapOf("value" to 1, "repeats" to 5, "counter" to 5, "result" to 5), finalStore)
    }

    @Test
    fun `loopy program 1 execution 3`() {
        val finalStore = loopyProgram1().execute(
            mapOf("value" to 0, "repeats" to 0),
        )
        assertEquals(mapOf("value" to 0, "repeats" to 0, "counter" to 0, "result" to 0), finalStore)
    }

    @Test
    fun `loopy program 1 toString`() {
        val expected = """
            result = 0
            counter = 0
            while (counter < repeats) {
                result = result + value
                counter = counter + 1
            }

        """.trimIndent()
        assertEquals(expected, loopyProgram1().toString())
    }

    @Test
    fun `loopy program 2 execution 1`() {
        val finalStore = loopyProgram2().execute(
            mapOf("m" to 0, "n" to 0, "c1" to 0, "c2" to 0),
        )
        assertEquals(mapOf("m" to 0, "n" to 0, "c1" to 0, "c2" to 0, "sum" to 0, "i" to 0), finalStore)
    }

    @Test
    fun `loopy program 2 execution 2`() {
        val finalStore = loopyProgram2().execute(
            mapOf("m" to 10, "n" to 20, "c1" to 0, "c2" to 0),
        )
        assertEquals(mapOf("m" to 10, "n" to 20, "c1" to 45, "c2" to 155, "sum" to 200, "i" to 10, "j" to 20), finalStore)
    }

    @Test
    fun `loopy program 2 toString`() {
        val expected = """
            i = 0
            while (i < m) {
                j = 0
                while (j < n) {
                    if (i > j) {
                        c1 = c1 + 1
                    } else {
                        c2 = c2 + 1
                    }
                    j = j + 1
                }
                i = i + 1
            }
            sum = c1 + c2

        """.trimIndent()
        assertEquals(expected, loopyProgram2().toString())
    }

    @Test
    fun `while statement toString`() {
        val stmt = Stmt.While(
            BoolExpr.LessThan(IntExpr.Var("i"), IntExpr.Var("n")),
            Stmt.Assign(
                "x",
                IntExpr.Add(IntExpr.Var("x"), IntExpr.Literal(1)),
                Stmt.Assign(
                    "y",
                    IntExpr.Sub(IntExpr.Var("y"), IntExpr.Literal(2)),
                    Stmt.Assign(
                        "z",
                        IntExpr.Mul(IntExpr.Var("z"), IntExpr.Literal(2)),
                        Stmt.Assign(
                            "i",
                            IntExpr.Add(IntExpr.Var("i"), IntExpr.Literal(1)),
                        ),
                    ),
                ),
            ),
            Stmt.While(
                BoolExpr.GreaterThan(IntExpr.Var("i"), IntExpr.Literal(0)),
                Stmt.Assign(
                    "x",
                    IntExpr.Sub(IntExpr.Var("x"), IntExpr.Literal(1)),
                    Stmt.Assign(
                        "y",
                        IntExpr.Add(IntExpr.Var("y"), IntExpr.Literal(2)),
                        Stmt.Assign(
                            "z",
                            IntExpr.Div(IntExpr.Var("z"), IntExpr.Literal(2)),
                            Stmt.Assign(
                                "i",
                                IntExpr.Sub(IntExpr.Var("i"), IntExpr.Literal(1)),
                            ),
                        ),
                    ),
                ),
            ),
        )
        val expected = """
            while (i < n) {
                x = x + 1
                y = y - 2
                z = z * 2
                i = i + 1
            }
            while (i > 0) {
                x = x - 1
                y = y + 2
                z = z / 2
                i = i - 1
            }

        """.trimIndent()
        assertEquals(expected, stmt.toString())
    }

    private fun loopyProgram1(): SequentialProgram = SequentialProgram(
        Stmt.Assign(
            "result",
            IntExpr.Literal(0),
            Stmt.Assign(
                "counter",
                IntExpr.Literal(0),
                Stmt.While(
                    BoolExpr.LessThan(
                        IntExpr.Var("counter"),
                        IntExpr.Var("repeats"),
                    ),
                    Stmt.Assign(
                        "result",
                        IntExpr.Add(
                            IntExpr.Var("result"),
                            IntExpr.Var("value"),
                        ),
                        Stmt.Assign(
                            "counter",
                            IntExpr.Add(
                                IntExpr.Var("counter"),
                                IntExpr.Literal(1),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )

    private fun loopyProgram2(): SequentialProgram = SequentialProgram(
        Stmt.Assign(
            "i",
            IntExpr.Literal(0),
            Stmt.While(
                BoolExpr.LessThan(IntExpr.Var("i"), IntExpr.Var("m")),
                Stmt.Assign(
                    "j",
                    IntExpr.Literal(0),
                    Stmt.While(
                        BoolExpr.LessThan(IntExpr.Var("j"), IntExpr.Var("n")),
                        Stmt.If(
                            BoolExpr.GreaterThan(IntExpr.Var("i"), IntExpr.Var("j")),
                            Stmt.Assign("c1", IntExpr.Add(IntExpr.Var("c1"), IntExpr.Literal(1))),
                            Stmt.Assign("c2", IntExpr.Add(IntExpr.Var("c2"), IntExpr.Literal(1))),
                            Stmt.Assign("j", IntExpr.Add(IntExpr.Var("j"), IntExpr.Literal(1))),
                        ),
                        Stmt.Assign("i", IntExpr.Add(IntExpr.Var("i"), IntExpr.Literal(1))),
                    ),
                ),
                Stmt.Assign(
                    "sum",
                    IntExpr.Add(
                        IntExpr.Var("c1"),
                        IntExpr.Var("c2"),
                    ),
                ),
            ),
        ),
    )
}
*/
