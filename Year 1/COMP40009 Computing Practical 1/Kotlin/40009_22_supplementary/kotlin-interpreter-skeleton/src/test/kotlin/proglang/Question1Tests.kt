package proglang

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertSame
import kotlin.test.fail

class Question1Tests {
    private val store: Map<String, Int> =
        mapOf(
            "a" to 1,
            "b" to -2,
            "c" to 3,
            "d" to -4,
            "e" to 5,
            "f" to -6,
            "g" to 7,
        )

    @Test
    fun `add literals`() {
        assertEquals(25, IntExpr.Add(IntExpr.Literal(5), IntExpr.Literal(20)).eval(store))
    }

    @Test
    fun `sub literals`() {
        assertEquals(-15, IntExpr.Sub(IntExpr.Literal(5), IntExpr.Literal(20)).eval(store))
    }

    @Test
    fun `mul literals`() {
        assertEquals(100, IntExpr.Mul(IntExpr.Literal(5), IntExpr.Literal(20)).eval(store))
    }

    @Test
    fun `div literals 1`() {
        assertEquals(4, IntExpr.Div(IntExpr.Literal(20), IntExpr.Literal(5)).eval(store))
    }

    @Test
    fun `div literals 2`() {
        assertEquals(4, IntExpr.Div(IntExpr.Literal(17), IntExpr.Literal(4)).eval(store))
    }

    @Test
    fun `fact literals`() {
        assertEquals(720, IntExpr.Fact(IntExpr.Literal(6)).eval(store))
    }

    @Test
    fun `var`() {
        assertEquals(1000, IntExpr.Var("foo").eval(mapOf("foo" to 1000)))
    }

    @Test
    fun literal() {
        assertEquals(42, IntExpr.Literal(42).eval(store))
    }

    @Test
    fun `add vars`() {
        assertEquals(-1, IntExpr.Add(IntExpr.Var("a"), IntExpr.Var("b")).eval(store))
    }

    @Test
    fun `sub vars`() {
        assertEquals(7, IntExpr.Sub(IntExpr.Var("c"), IntExpr.Var("d")).eval(store))
    }

    @Test
    fun `mul vars`() {
        assertEquals(-30, IntExpr.Mul(IntExpr.Var("e"), IntExpr.Var("f")).eval(store))
    }

    @Test
    fun `div 1 vars`() {
        assertEquals(3, IntExpr.Div(IntExpr.Var("f"), IntExpr.Var("b")).eval(store))
    }

    @Test
    fun `div 2 vars`() {
        assertEquals(2, IntExpr.Div(IntExpr.Var("g"), IntExpr.Var("c")).eval(store))
    }

    @Test
    fun `fact var`() {
        assertEquals(120, IntExpr.Fact(IntExpr.Var("e")).eval(store))
    }

    @Test
    fun `var exception`() {
        try {
            IntExpr.Var("x").eval(store)
            fail("Expected UndefinedBehaviourException")
        } catch (_: UndefinedBehaviourException) {
            // Good: an exception was expected
        }
    }

    @Test
    fun `fact exception`() {
        try {
            IntExpr.Fact(IntExpr.Var("b")).eval(store)
            fail("Expected UndefinedBehaviourException")
        } catch (_: UndefinedBehaviourException) {
            // Good: an exception was expected
        }
    }

    @Test
    fun `div exception`() {
        try {
            IntExpr.Div(IntExpr.Var("c"), IntExpr.Literal(0)).eval(store)
            fail("Expected UndefinedBehaviourException")
        } catch (_: UndefinedBehaviourException) {
            // Good: an exception was expected
        }
    }

    @Test
    fun `complex expression`() {
        // ((b / a) * b)! + (e - f) * 14
        val a = IntExpr.Var("a")
        val b = IntExpr.Var("b")
        val e = IntExpr.Var("e")
        val f = IntExpr.Var("f")
        val `14` = IntExpr.Literal(14)
        val `b div a` = IntExpr.Div(b, a)
        val `(b div a)` = IntExpr.Paren(`b div a`)
        val `(b div a) mul b` = IntExpr.Mul(`(b div a)`, b)
        val `((b div a) mul b)` = IntExpr.Paren(`(b div a) mul b`)
        val `((b div a) mul b)!` = IntExpr.Fact(`((b div a) mul b)`)
        val `e - f` = IntExpr.Sub(e, f)
        val `(e - f)` = IntExpr.Paren(`e - f`)
        val `(e - f) mul 14` = IntExpr.Mul(`(e - f)`, `14`)
        val `((b div a) mul b)! + (e - f) mul 14` = IntExpr.Add(`((b div a) mul b)!`, `(e - f) mul 14`)
        assertEquals(178, `((b div a) mul b)! + (e - f) mul 14`.eval(store))
        assertSame(`((b div a) mul b)!`, `((b div a) mul b)! + (e - f) mul 14`.lhs)
        assertSame(`((b div a) mul b)`, `((b div a) mul b)!`.expr)
        assertSame(`(b div a) mul b`, `((b div a) mul b)`.expr)
        assertSame(`(b div a)`, `(b div a) mul b`.lhs)
        assertSame(`b div a`, `(b div a)`.expr)
        assertSame(b, `b div a`.lhs)
        assertSame(a, `b div a`.rhs)
        assertSame(b, `(b div a) mul b`.rhs)
        assertSame(`(e - f) mul 14`, `((b div a) mul b)! + (e - f) mul 14`.rhs)
        assertSame(`(e - f)`, `(e - f) mul 14`.lhs)
        assertSame(`e - f`, `(e - f)`.expr)
        assertSame(e, `e - f`.lhs)
        assertSame(f, `e - f`.rhs)
        assertSame(`14`, `(e - f) mul 14`.rhs)
        assertEquals("a", a.name)
        assertEquals("b", b.name)
        assertEquals("e", e.name)
        assertEquals("f", f.name)
        assertEquals(14, `14`.value)
    }

    @Test
    fun `add literals toString`() {
        assertEquals("5 + 20", IntExpr.Add(IntExpr.Literal(5), IntExpr.Literal(20)).toString())
    }

    @Test
    fun `sub literals toString`() {
        assertEquals("5 - 20", IntExpr.Sub(IntExpr.Literal(5), IntExpr.Literal(20)).toString())
    }

    @Test
    fun `mul literals toString`() {
        assertEquals("5 * 20", IntExpr.Mul(IntExpr.Literal(5), IntExpr.Literal(20)).toString())
    }

    @Test
    fun `div literals 1 toString`() {
        assertEquals("20 / 5", IntExpr.Div(IntExpr.Literal(20), IntExpr.Literal(5)).toString())
    }

    @Test
    fun `div literals 2 toString`() {
        assertEquals("17 / 4", IntExpr.Div(IntExpr.Literal(17), IntExpr.Literal(4)).toString())
    }

    @Test
    fun `fact literals toString`() {
        assertEquals("6!", IntExpr.Fact(IntExpr.Literal(6)).toString())
    }

    @Test
    fun `var toString`() {
        assertEquals("foo", IntExpr.Var("foo").toString())
    }

    @Test
    fun `literal toString`() {
        assertEquals("42", IntExpr.Literal(42).toString())
    }

    @Test
    fun `add vars toString`() {
        assertEquals("a + b", IntExpr.Add(IntExpr.Var("a"), IntExpr.Var("b")).toString())
    }

    @Test
    fun `sub vars toString`() {
        assertEquals("c - d", IntExpr.Sub(IntExpr.Var("c"), IntExpr.Var("d")).toString())
    }

    @Test
    fun `mul vars toString`() {
        assertEquals("e * f", IntExpr.Mul(IntExpr.Var("e"), IntExpr.Var("f")).toString())
    }

    @Test
    fun `div 1 vars toString`() {
        assertEquals("f / b", IntExpr.Div(IntExpr.Var("f"), IntExpr.Var("b")).toString())
    }

    @Test
    fun `div 2 vars toString`() {
        assertEquals("g / c", IntExpr.Div(IntExpr.Var("g"), IntExpr.Var("c")).toString())
    }

    @Test
    fun `fact var toString`() {
        assertEquals("e!", IntExpr.Fact(IntExpr.Var("e")).toString())
    }

    @Test
    fun `div zero  toString`() {
        assertEquals("c / 0", IntExpr.Div(IntExpr.Var("c"), IntExpr.Literal(0)).toString())
    }

    @Test
    fun `complex expression toString`() {
        // ((b / a) * b)! + (e - f) * 14
        val a = IntExpr.Var("a")
        val b = IntExpr.Var("b")
        val e = IntExpr.Var("e")
        val f = IntExpr.Var("f")
        val `14` = IntExpr.Literal(14)
        val `b div a` = IntExpr.Div(b, a)
        val `(b div a)` = IntExpr.Paren(`b div a`)
        val `(b div a) mul b` = IntExpr.Mul(`(b div a)`, b)
        val `((b div a) mul b)` = IntExpr.Paren(`(b div a) mul b`)
        val `((b div a) mul b)!` = IntExpr.Fact(`((b div a) mul b)`)
        val `e - f` = IntExpr.Sub(e, f)
        val `(e - f)` = IntExpr.Paren(`e - f`)
        val `(e - f) mul 14` = IntExpr.Mul(`(e - f)`, `14`)
        val `((b div a) mul b)! + (e - f) mul 14` = IntExpr.Add(`((b div a) mul b)!`, `(e - f) mul 14`)
        assertEquals("((b / a) * b)! + (e - f) * 14", `((b div a) mul b)! + (e - f) mul 14`.toString())
    }
}
