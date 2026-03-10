package proglang

/*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue
import kotlin.test.fail

class Question7Tests {

    @Test
    fun `exception if durations and pauses do not match`() {
        try {
            ConcurrentProgram(
                listOf(senderThreadBody(), receiverThreadBody()),
                listOf(10),
            )
            fail("IllegalArgumentException was expected.")
        } catch (_: IllegalArgumentException) {
            // Good: exception was expected.
        }
    }

    @Test
    fun `message passing 1`() {
        val program = ConcurrentProgram(
            listOf(senderThreadBody(), receiverThreadBody()),
            listOf(10, 5),
        )
        for (repeats in 1..10) {
            val finalStore = program.execute(mapOf("data" to 0, "flag" to 0))
            assertEquals(mapOf("data" to 1, "flag" to 1, "observed" to 1), finalStore)
        }
    }

    @Test
    fun `message passing 2`() {
        val program = ConcurrentProgram(
            listOf(senderThreadBody(), receiverThreadBody()),
            listOf(5, 10),
        )
        for (repeats in 1..10) {
            val finalStore = program.execute(mapOf("data" to 0, "flag" to 0))
            assertEquals(mapOf("data" to 1, "flag" to 1, "observed" to 1), finalStore)
        }
    }

    @Test
    fun `lots of additions 1`() {
        val program = ConcurrentProgram(
            (0..<8).map { lotsOfAdditionsThreadBody() },
            (0..<8).map { 1 },
        )
        for (repeats in 1..10) {
            val finalStore = program.execute(mapOf("a" to 0, "b" to 0, "c" to 0))
            assertEquals(mapOf("a" to 8, "b" to 16, "c" to 24), finalStore)
        }
    }

    @Test
    fun `lots of additions 2`() {
        val program = ConcurrentProgram(
            (0..<8).map { lotsOfAdditionsThreadBody() },
            (0..<8).map { it.toLong() },
        )
        for (repeats in 1..10) {
            val finalStore = program.execute(mapOf("a" to 4, "b" to 4, "c" to 4))
            assertEquals(mapOf("a" to 12, "b" to 20, "c" to 28), finalStore)
        }
    }

    @Test
    fun `zero sum game 1`() {
        val program = ConcurrentProgram(
            (0..<4).map { zeroSumGameThreadBody("counter$it") },
            (0..<4).map { it.toLong() },
        )
        for (repeats in 1..10) {
            val finalStore = program.execute(mapOf("n" to 4, "x" to 3, "y" to 2, "z" to 1))
            val expectedFinalStore = mutableMapOf("n" to 4, "x" to 3, "y" to 2, "z" to 1)
            (0..<4).forEach { expectedFinalStore["counter$it"] = 0 }
            assertEquals(expectedFinalStore, finalStore)
        }
    }

    @Test
    fun `zero sum game 2`() {
        val program = ConcurrentProgram(
            (0..<4).map { zeroSumGameThreadBody("counter$it") },
            (0..<4).map { 1 },
        )
        for (repeats in 1..10) {
            val finalStore = program.execute(mapOf("n" to 5, "x" to 7, "y" to 6, "z" to 1))
            val expectedFinalStore = mutableMapOf("n" to 5, "x" to 7, "y" to 6, "z" to 1)
            (0..<4).forEach { expectedFinalStore["counter$it"] = 0 }
            assertEquals(expectedFinalStore, finalStore)
        }
    }

    @Test
    fun `store buffering 1`() {
        val program = ConcurrentProgram(
            listOf(storeBufferingThread1Body(), storeBufferingThread2Body()),
            listOf(1, 5),
        )
        val acceptableFinalStores = setOf(
            mapOf("x" to 1, "y" to 1, "r0" to 1, "r1" to 0),
            mapOf("x" to 1, "y" to 1, "r0" to 0, "r1" to 1),
            mapOf("x" to 1, "y" to 1, "r0" to 1, "r1" to 1),
        )

        for (repeats in 1..100) {
            val finalStore = program.execute(mapOf("x" to 0, "y" to 0))
            assertTrue(finalStore in acceptableFinalStores)
        }
    }

    @Test
    fun `store buffering 2`() {
        val program = ConcurrentProgram(
            listOf(storeBufferingThread1Body(), storeBufferingThread2Body()),
            listOf(5, 1),
        )
        val acceptableFinalStores = setOf(
            mapOf("x" to 1, "y" to 1, "r0" to 1, "r1" to 0),
            mapOf("x" to 1, "y" to 1, "r0" to 0, "r1" to 1),
            mapOf("x" to 1, "y" to 1, "r0" to 1, "r1" to 1),
        )

        for (repeats in 1..100) {
            val finalStore = program.execute(mapOf("x" to 0, "y" to 0))
            assertTrue(finalStore in acceptableFinalStores)
        }
    }

    @Test
    fun `store buffering 3`() {
        val program = ConcurrentProgram(
            listOf(storeBufferingThread1Body(), storeBufferingThread2Body()),
            listOf(1, 1),
        )
        val acceptableFinalStores = setOf(
            mapOf("x" to 1, "y" to 1, "r0" to 1, "r1" to 0),
            mapOf("x" to 1, "y" to 1, "r0" to 0, "r1" to 1),
            mapOf("x" to 1, "y" to 1, "r0" to 1, "r1" to 1),
        )

        for (repeats in 1..100) {
            val finalStore = program.execute(mapOf("x" to 0, "y" to 0))
            assertTrue(finalStore in acceptableFinalStores)
        }
    }

    private fun senderThreadBody(): Stmt =
        Stmt.Assign(
            "data",
            IntExpr.Literal(1),
            Stmt.Assign(
                "flag",
                IntExpr.Literal(1),
            ),
        )

    private fun receiverThreadBody(): Stmt =
        Stmt.While(
            BoolExpr.Equals(IntExpr.Var("flag"), IntExpr.Literal(0)),
            null,
            Stmt.Assign(
                "observed",
                IntExpr.Var("data"),
            ),
        )

    private fun lotsOfAdditionsThreadBody(): Stmt =
        Stmt.Assign(
            "a",
            IntExpr.Add(IntExpr.Var("a"), IntExpr.Literal(1)),
            Stmt.Assign(
                "b",
                IntExpr.Add(IntExpr.Var("b"), IntExpr.Literal(2)),
                Stmt.Assign(
                    "c",
                    IntExpr.Add(IntExpr.Var("c"), IntExpr.Literal(3)),
                ),
            ),
        )

    private fun zeroSumGameThreadBody(counterName: String): Stmt =
        Stmt.Assign(
            counterName,
            IntExpr.Literal(0),
            Stmt.While(
                BoolExpr.LessThan(IntExpr.Var(counterName), IntExpr.Var("n")),
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
                                counterName,
                                IntExpr.Add(IntExpr.Var(counterName), IntExpr.Literal(1)),
                            ),
                        ),
                    ),
                ),
                Stmt.While(
                    BoolExpr.GreaterThan(IntExpr.Var(counterName), IntExpr.Literal(0)),
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
                                    counterName,
                                    IntExpr.Sub(IntExpr.Var(counterName), IntExpr.Literal(1)),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        )

    private fun storeBufferingThread1Body(): Stmt =
        Stmt.Assign(
            "x",
            IntExpr.Literal(1),
            Stmt.Assign(
                "r0",
                IntExpr.Var("y"),
            ),
        )

    private fun storeBufferingThread2Body(): Stmt =
        Stmt.Assign(
            "y",
            IntExpr.Literal(1),
            Stmt.Assign(
                "r1",
                IntExpr.Var("x"),
            ),
        )
}
*/
