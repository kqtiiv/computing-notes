package tunes

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertSame
import kotlin.test.assertTrue

class Question2Tests {
    @Test
    fun testConstructor() {
        val tune: Tune = StandardTune()
        assertTrue(tune.notes.isEmpty())
    }

    @Test
    fun testAddNote() {
        val tune: Tune = StandardTune()
        val note1 = Note(1, 4.0)
        val note2 = Note(2, 5.0)
        val note3 = Note(3, 6.0)
        val note4 = Note(4, 7.0)
        tune.addNote(note1)
        assertEquals(1, tune.notes.size)
        assertSame(note1, tune.notes[0])
        tune.addNote(note2)
        assertEquals(2, tune.notes.size)
        assertSame(note1, tune.notes[0])
        assertSame(note2, tune.notes[1])
        tune.addNote(note3)
        assertEquals(3, tune.notes.size)
        assertSame(note1, tune.notes[0])
        assertSame(note2, tune.notes[1])
        assertSame(note3, tune.notes[2])
        tune.addNote(note4)
        assertEquals(4, tune.notes.size)
        assertSame(note1, tune.notes[0])
        assertSame(note2, tune.notes[1])
        assertSame(note3, tune.notes[2])
        assertSame(note4, tune.notes[3])
    }

    @Test
    fun testGetTotalDuration1() {
        assertEquals(0.0, StandardTune().totalDuration)
    }

    @Test
    fun testGetTotalDuration2() {
        val tune: Tune = StandardTune()
        tune.addNote(Note(10, 10.0))
        tune.addNote(Note(10, 2.0))
        tune.addNote(Note(10, 7.0))
        tune.addNote(Note(10, 15.0))
        assertEquals(34.0, tune.totalDuration)
    }

    @Test
    fun testGetTotalDuration3() {
        val tune: Tune = StandardTune()
        for (i in 1..29) {
            tune.addNote(Note(0, i.toDouble()))
        }
        assertEquals(435.0, tune.totalDuration)
    }

    @Test
    fun testEmptyIteration() {
        val expectedNotes: List<Note> =
            listOf()
        val actualNotes: MutableList<Note> = mutableListOf()
        val tune: Tune = StandardTune()

        for (note in tune) {
            actualNotes.add(note)
        }
        assertEquals(expectedNotes, actualNotes)
        assertEquals(expectedNotes, tune.notes)
    }

    @Test
    fun testIteration() {
        val expectedNotes =
            listOf(
                Note(10, 10.0),
                Note(10, 2.0),
                Note(10, 7.0),
                Note(10, 15.0),
            )
        val actualNotes: MutableList<Note> = mutableListOf()
        val tune: Tune = StandardTune()
        tune.addNote(Note(10, 10.0))
        tune.addNote(Note(10, 2.0))
        tune.addNote(Note(10, 7.0))
        tune.addNote(Note(10, 15.0))
        for (note in tune) {
            actualNotes.add(note)
        }
        assertEquals(expectedNotes, actualNotes)
        assertEquals(expectedNotes, tune.notes)
    }
}
