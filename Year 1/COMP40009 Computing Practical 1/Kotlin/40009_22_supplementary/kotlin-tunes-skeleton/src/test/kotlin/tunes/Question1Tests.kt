package tunes

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotEquals
import kotlin.test.assertTrue
import kotlin.test.fail

class Question1Tests {
    @Test
    fun testPitch() {
        assertEquals(5, Note(5, 10.0).pitch)
    }

    @Test
    fun testDuration() {
        assertEquals(10.0, Note(5, 10.0).duration)
    }

    @Test
    fun testConstructorNegativePitch() {
        try {
            Note(-1, 2.0)
            fail()
        } catch (exception: IllegalArgumentException) {
            // Good - an exception should have been thrown.
        }
    }

    @Test
    fun testConstructorPitchTooHigh1() {
        try {
            Note(201, 8.0)
            fail()
        } catch (exception: IllegalArgumentException) {
            // Good - an exception should have been thrown.
        }
    }

    @Test
    fun testConstructorPitchTooHigh2() {
        try {
            Note(5000, 8.0)
            fail()
        } catch (exception: IllegalArgumentException) {
            // Good - an exception should have been thrown.
        }
    }

    @Test
    fun testConstructorZeroDuration() {
        try {
            Note(5, 0.0)
            fail()
        } catch (exception: IllegalArgumentException) {
            // Good - an exception should have been thrown.
        }
    }

    @Test
    fun testConstructorNegativeDuration() {
        try {
            Note(8, -20.0)
            fail()
        } catch (exception: IllegalArgumentException) {
            // Good - an exception should have been thrown.
        }
    }

    @Test
    fun testConstructorDurationTooHigh1() {
        try {
            Note(80, 65.0)
            fail()
        } catch (exception: IllegalArgumentException) {
            // Good - an exception should have been thrown.
        }
    }

    @Test
    fun testConstructorDurationTooHigh2() {
        try {
            Note(80, 1000.0)
            fail()
        } catch (exception: IllegalArgumentException) {
            // Good - an exception should have been thrown.
        }
    }

    @Test
    fun testConstructorBottomShortestNote() {
        Note(0, 1.0)
    }

    @Test
    fun testConstructorTopFastestNote() {
        Note(200, 64.0)
    }

    @Test
    fun testToString() {
        assertEquals("C0(1.0)", Note(0, 1.0).toString())
        assertEquals("C#0(2.0)", Note(1, 2.0).toString())
        assertEquals("D0(3.0)", Note(2, 3.0).toString())
        assertEquals("D#0(4.0)", Note(3, 4.0).toString())
        assertEquals("E0(5.0)", Note(4, 5.0).toString())
        assertEquals("F0(6.0)", Note(5, 6.0).toString())
        assertEquals("F#0(7.0)", Note(6, 7.0).toString())
        assertEquals("G0(8.0)", Note(7, 8.0).toString())
        assertEquals("G#0(9.0)", Note(8, 9.0).toString())
        assertEquals("A0(10.0)", Note(9, 10.0).toString())
        assertEquals("A#0(11.0)", Note(10, 11.0).toString())
        assertEquals("B0(12.0)", Note(11, 12.0).toString())
        assertEquals("C1(13.0)", Note(12, 13.0).toString())
        assertEquals("C#1(14.0)", Note(13, 14.0).toString())
        assertEquals("D1(15.0)", Note(14, 15.0).toString())
        assertEquals("D#1(16.0)", Note(15, 16.0).toString())
        assertEquals("E1(17.0)", Note(16, 17.0).toString())
        assertEquals("F1(18.0)", Note(17, 18.0).toString())
        assertEquals("F#1(19.0)", Note(18, 19.0).toString())
        assertEquals("G1(20.0)", Note(19, 20.0).toString())
        assertEquals("G#1(21.0)", Note(20, 21.0).toString())
        assertEquals("A1(22.0)", Note(21, 22.0).toString())
        assertEquals("A#1(23.0)", Note(22, 23.0).toString())
        assertEquals("B1(24.0)", Note(23, 24.0).toString())
        assertEquals("C4(13.0)", Note(48, 13.0).toString())
        assertEquals("C#4(14.0)", Note(49, 14.0).toString())
        assertEquals("D4(15.0)", Note(50, 15.0).toString())
        assertEquals("D#4(16.0)", Note(51, 16.0).toString())
        assertEquals("E4(17.0)", Note(52, 17.0).toString())
        assertEquals("F4(18.0)", Note(53, 18.0).toString())
        assertEquals("F#4(19.0)", Note(54, 19.0).toString())
        assertEquals("G4(20.0)", Note(55, 20.0).toString())
        assertEquals("G#4(21.0)", Note(56, 21.0).toString())
        assertEquals("A4(22.0)", Note(57, 22.0).toString())
        assertEquals("A#4(23.0)", Note(58, 23.0).toString())
        assertEquals("B4(24.0)", Note(59, 24.0).toString())
    }

    @Test
    fun testEquals1() {
        val note = Note(1, 3.0)
        assertEquals(note, note)
    }

    @Test
    fun testEquals2() {
        val note1 = Note(1, 3.0)
        val note2 = Note(1, 3.0)
        assertEquals(note1, note2)
    }

    @Test
    fun testEquals3() {
        val note1 = Note(1, 3.0)
        val note2 = Note(2, 3.0)
        assertNotEquals(note1, note2)
    }

    @Test
    fun testEquals4() {
        val note1 = Note(1, 3.0)
        val note2 = Note(1, 4.0)
        assertNotEquals(note1, note2)
    }

    @Test
    fun testEquals5() {
        val note = Note(1, 3.0)
        assertNotEquals<Note?>(note, null)
    }

    @Test
    fun testEquals6() {
        val note = Note(1, 3.0)
        assertNotEquals<Any>(note, note.toString())
    }

    @Test
    fun testEquals7() {
        val note1 = Note(1, 3.0)
        val note2 = Note(1, 3.0)
        val notes: MutableSet<Note> = HashSet()
        notes.add(note1)
        assertTrue(notes.contains(note2))
    }

    @Test
    fun testHasNoteAbove1() {
        assertTrue(Note(0, 1.0).hasNoteAbove())
    }

    @Test
    fun testHasNoteAbove2() {
        assertTrue(Note(199, 1.0).hasNoteAbove())
    }

    @Test
    fun testHasNoteAbove3() {
        assertFalse(Note(200, 1.0).hasNoteAbove())
    }

    @Test
    fun testHasNoteBelow1() {
        assertTrue(Note(1, 1.0).hasNoteBelow())
    }

    @Test
    fun testHasNoteBelow2() {
        assertTrue(Note(8, 1.0).hasNoteBelow())
    }

    @Test
    fun testHasNoteBelow3() {
        assertFalse(Note(0, 1.0).hasNoteBelow())
    }

    @Test
    fun testNoteAbove1() {
        assertEquals(Note(1, 1.0), Note(0, 1.0).noteAbove())
    }

    @Test
    fun testNoteAbove2() {
        assertEquals(Note(200, 1.0), Note(199, 1.0).noteAbove())
    }

    @Test
    fun testNoteBelow1() {
        assertEquals(Note(0, 1.0), Note(1, 1.0).noteBelow())
    }

    @Test
    fun testNoteBelow2() {
        assertEquals(Note(7, 1.0), Note(8, 1.0).noteBelow())
    }
}
