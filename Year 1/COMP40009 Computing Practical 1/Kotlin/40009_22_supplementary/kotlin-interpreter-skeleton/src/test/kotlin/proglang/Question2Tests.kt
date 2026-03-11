package proglang

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue
import kotlin.test.fail

class Question2Tests {
    private val store: Map<String, Int> =
        mapOf(
            "a" to 1,
            "b" to -2,
            "c" to 3,
            "d" to -4,
            "e" to 5,
            "f" to -6,
            "g" to 7,
            "h" to 0,
        )

    @Test
    fun `less than`() {
        assertTrue(BoolExpr.LessThan(IntExpr.Literal(1), IntExpr.Literal(2)).eval(store))
        assertFalse(BoolExpr.LessThan(IntExpr.Literal(1), IntExpr.Literal(1)).eval(store))
    }

    @Test
    fun `greater than`() {
        assertTrue(BoolExpr.GreaterThan(IntExpr.Literal(10), IntExpr.Literal(2)).eval(store))
        assertFalse(BoolExpr.GreaterThan(IntExpr.Literal(10), IntExpr.Literal(10)).eval(store))
    }

    @Test
    fun equals() {
        assertTrue(BoolExpr.Equals(IntExpr.Literal(1), IntExpr.Var("a")).eval(store))
        assertFalse(BoolExpr.Equals(IntExpr.Var("f"), IntExpr.Var("a")).eval(store))
    }

    @Test
    fun and() {
        val trueExpr = BoolExpr.GreaterThan(IntExpr.Literal(10), IntExpr.Literal(2))
        val falseExpr = BoolExpr.Equals(IntExpr.Var("f"), IntExpr.Var("a"))
        assertFalse(BoolExpr.And(falseExpr, falseExpr).eval(store))
        assertFalse(BoolExpr.And(falseExpr, trueExpr).eval(store))
        assertFalse(BoolExpr.And(trueExpr, falseExpr).eval(store))
        assertTrue(BoolExpr.And(trueExpr, trueExpr).eval(store))
    }

    @Test
    fun or() {
        val trueExpr = BoolExpr.GreaterThan(IntExpr.Literal(10), IntExpr.Literal(2))
        val falseExpr = BoolExpr.Equals(IntExpr.Var("f"), IntExpr.Var("a"))
        assertFalse(BoolExpr.Or(falseExpr, falseExpr).eval(store))
        assertTrue(BoolExpr.Or(falseExpr, trueExpr).eval(store))
        assertTrue(BoolExpr.Or(trueExpr, falseExpr).eval(store))
        assertTrue(BoolExpr.Or(trueExpr, trueExpr).eval(store))
    }

    @Test
    fun not() {
        val trueExpr = BoolExpr.GreaterThan(IntExpr.Literal(10), IntExpr.Literal(2))
        val falseExpr = BoolExpr.Equals(IntExpr.Var("f"), IntExpr.Var("a"))
        assertTrue(BoolExpr.Not(BoolExpr.Paren(falseExpr)).eval(store))
        assertFalse(BoolExpr.Not(BoolExpr.Paren(trueExpr)).eval(store))
    }

    @Test
    fun paren() {
        val trueExpr = BoolExpr.GreaterThan(IntExpr.Literal(10), IntExpr.Literal(2))
        val falseExpr = BoolExpr.Equals(IntExpr.Var("f"), IntExpr.Var("a"))
        assertTrue(BoolExpr.Paren(trueExpr).eval(store))
        assertFalse(BoolExpr.Paren(falseExpr).eval(store))
    }

    @Test
    fun `and short circuit`() {
        assertFalse(
            BoolExpr
                .And(
                    BoolExpr.Not(BoolExpr.Paren(BoolExpr.Equals(IntExpr.Var("h"), IntExpr.Literal(0)))),
                    BoolExpr.Equals(IntExpr.Literal(1), IntExpr.Div(IntExpr.Var("a"), IntExpr.Var("h"))),
                ).eval(store),
        )
    }

    @Test
    fun `and short circuit exception`() {
        try {
            BoolExpr
                .And(
                    BoolExpr.Equals(IntExpr.Var("h"), IntExpr.Literal(0)),
                    BoolExpr.Equals(IntExpr.Literal(1), IntExpr.Div(IntExpr.Var("a"), IntExpr.Var("h"))),
                ).eval(store)
            fail("Expected UndefinedBehaviourException")
        } catch (_: UndefinedBehaviourException) {
            // Good: an exception was expected.
        }
    }

    @Test
    fun `or short circuit`() {
        assertTrue(
            BoolExpr
                .Or(
                    BoolExpr.Equals(IntExpr.Var("h"), IntExpr.Literal(0)),
                    BoolExpr.Equals(IntExpr.Literal(1), IntExpr.Div(IntExpr.Var("a"), IntExpr.Var("h"))),
                ).eval(store),
        )
    }

    @Test
    fun `or short circuit exception`() {
        try {
            BoolExpr
                .Or(
                    BoolExpr.Not(BoolExpr.Equals(IntExpr.Var("h"), IntExpr.Literal(0))),
                    BoolExpr.Equals(IntExpr.Literal(1), IntExpr.Div(IntExpr.Var("a"), IntExpr.Var("h"))),
                ).eval(store)
            fail("Expected UndefinedBehaviourException")
        } catch (_: UndefinedBehaviourException) {
            // Good: an exception was expected.
        }
    }

    @Test
    fun `less than toString`() {
        assertEquals("1 < 2", BoolExpr.LessThan(IntExpr.Literal(1), IntExpr.Literal(2)).toString())
        assertEquals("1 < 1", BoolExpr.LessThan(IntExpr.Literal(1), IntExpr.Literal(1)).toString())
    }

    @Test
    fun `greater than toString`() {
        assertEquals("10 > 2", BoolExpr.GreaterThan(IntExpr.Literal(10), IntExpr.Literal(2)).toString())
        assertEquals("10 > 10", BoolExpr.GreaterThan(IntExpr.Literal(10), IntExpr.Literal(10)).toString())
    }

    @Test
    fun `equals toString`() {
        assertEquals("1 == a", BoolExpr.Equals(IntExpr.Literal(1), IntExpr.Var("a")).toString())
        assertEquals("f == a", BoolExpr.Equals(IntExpr.Var("f"), IntExpr.Var("a")).toString())
    }

    @Test
    fun `and toString`() {
        val trueExpr = BoolExpr.GreaterThan(IntExpr.Literal(10), IntExpr.Literal(2))
        val falseExpr = BoolExpr.Equals(IntExpr.Var("f"), IntExpr.Var("a"))
        assertEquals("f == a && f == a", BoolExpr.And(falseExpr, falseExpr).toString())
        assertEquals("f == a && 10 > 2", BoolExpr.And(falseExpr, trueExpr).toString())
        assertEquals("10 > 2 && f == a", BoolExpr.And(trueExpr, falseExpr).toString())
        assertEquals("10 > 2 && 10 > 2", BoolExpr.And(trueExpr, trueExpr).toString())
    }

    @Test
    fun `or toString`() {
        val trueExpr = BoolExpr.GreaterThan(IntExpr.Literal(10), IntExpr.Literal(2))
        val falseExpr = BoolExpr.Equals(IntExpr.Var("f"), IntExpr.Var("a"))
        assertEquals("f == a || f == a", BoolExpr.Or(falseExpr, falseExpr).toString())
        assertEquals("f == a || 10 > 2", BoolExpr.Or(falseExpr, trueExpr).toString())
        assertEquals("10 > 2 || f == a", BoolExpr.Or(trueExpr, falseExpr).toString())
        assertEquals("10 > 2 || 10 > 2", BoolExpr.Or(trueExpr, trueExpr).toString())
    }

    @Test
    fun `not toString`() {
        val trueExpr = BoolExpr.GreaterThan(IntExpr.Literal(10), IntExpr.Literal(2))
        val falseExpr = BoolExpr.Equals(IntExpr.Var("f"), IntExpr.Var("a"))
        assertEquals("!(f == a)", BoolExpr.Not(BoolExpr.Paren(falseExpr)).toString())
        assertEquals("!(10 > 2)", BoolExpr.Not(BoolExpr.Paren(trueExpr)).toString())
    }

    @Test
    fun `paren toString`() {
        val trueExpr = BoolExpr.GreaterThan(IntExpr.Literal(10), IntExpr.Literal(2))
        val falseExpr = BoolExpr.Equals(IntExpr.Var("f"), IntExpr.Var("a"))
        assertEquals("(10 > 2)", BoolExpr.Paren(trueExpr).toString())
        assertEquals("(f == a)", BoolExpr.Paren(falseExpr).toString())
    }

    @Test
    fun `and short circuit toString`() {
        assertEquals(
            "!(h == 0) && 1 == a / h",
            BoolExpr
                .And(
                    BoolExpr.Not(BoolExpr.Paren(BoolExpr.Equals(IntExpr.Var("h"), IntExpr.Literal(0)))),
                    BoolExpr.Equals(IntExpr.Literal(1), IntExpr.Div(IntExpr.Var("a"), IntExpr.Var("h"))),
                ).toString(),
        )
    }

    @Test
    fun `or short circuit toString`() {
        assertEquals(
            "h == 0 || 1 == a / h",
            BoolExpr
                .Or(
                    BoolExpr.Equals(IntExpr.Var("h"), IntExpr.Literal(0)),
                    BoolExpr.Equals(IntExpr.Literal(1), IntExpr.Div(IntExpr.Var("a"), IntExpr.Var("h"))),
                ).toString(),
        )
    }
}
