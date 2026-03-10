package tunes

import kotlin.test.Test
import kotlin.test.assertEquals

class Question3Tests {
    private fun simpleTuneNotes(): List<Note> =
        listOf(
            Note(1, 2.0),
            Note(10, 4.0),
            Note(20, 8.0),
            Note(40, 12.0),
        )

    private fun longerTuneNotes(): List<Note> {
        val result: MutableList<Note> =
            mutableListOf(
                Note(1, 2.0),
                Note(10, 4.0),
                Note(20, 8.0),
                Note(40, 12.0),
                Note(50, 2.0),
                Note(60, 4.0),
                Note(70, 8.0),
                Note(80, 12.0),
            )
        return result
    }

    private fun simpleTune(): Tune {
        val result: Tune = StandardTune()
        simpleTuneNotes().forEach({ result.addNote(it) })
        return result
    }

    @Test
    fun testTransposeUp() {
        val originalTune = simpleTune()
        val transposedUp: Tune = TransposedTune(originalTune, 2)
        assertEquals(simpleTuneNotes(), originalTune.notes)
        assertEquals(
            listOf(
                Note(3, 2.0),
                Note(12, 4.0),
                Note(22, 8.0),
                Note(42, 12.0),
            ),
            transposedUp.notes,
        )
    }

    @Test
    fun testTransposeDown() {
        val originalTune = simpleTune()
        val transposedDown: Tune = TransposedTune(originalTune, -1)
        assertEquals(simpleTuneNotes(), originalTune.notes)
        assertEquals(
            listOf(
                Note(0, 2.0),
                Note(9, 4.0),
                Note(19, 8.0),
                Note(39, 12.0),
            ),
            transposedDown.notes,
        )
    }

    @Test
    fun testTransposeUpThenDown() {
        val originalTune = simpleTune()
        val transposedUp: Tune = TransposedTune(originalTune, 1)
        val transposedDown: Tune = TransposedTune(transposedUp, -1)
        assertEquals(simpleTuneNotes(), originalTune.notes)
        assertEquals(simpleTuneNotes(), transposedDown.notes)
    }

    @Test
    fun testTransposeUpTooFar() {
        val originalTune = simpleTune()
        val transposedUp: Tune = TransposedTune(originalTune, 190)
        assertEquals(simpleTuneNotes(), originalTune.notes)
        assertEquals(
            listOf(
                Note(191, 2.0),
                Note(200, 4.0),
                Note(200, 8.0),
                Note(200, 12.0),
            ),
            transposedUp.notes,
        )
    }

    @Test
    fun testTransposeDownTooFar() {
        val originalTune = simpleTune()
        val transposedDown: Tune = TransposedTune(originalTune, -20)
        assertEquals(simpleTuneNotes(), originalTune.notes)
        assertEquals(
            listOf(
                Note(0, 2.0),
                Note(0, 4.0),
                Note(0, 8.0),
                Note(20, 12.0),
            ),
            transposedDown.notes,
        )
    }

    @Test
    fun testAddNotesAllOk1() {
        val originalTune = simpleTune()
        val transposedUp: Tune = TransposedTune(originalTune, 2)
        transposedUp.addNote(Note(52, 2.0))
        transposedUp.addNote(Note(62, 4.0))
        transposedUp.addNote(Note(72, 8.0))
        transposedUp.addNote(Note(82, 12.0))
        assertEquals(longerTuneNotes(), originalTune.notes)
        assertEquals(
            listOf(
                Note(3, 2.0),
                Note(12, 4.0),
                Note(22, 8.0),
                Note(42, 12.0),
                Note(52, 2.0),
                Note(62, 4.0),
                Note(72, 8.0),
                Note(82, 12.0),
            ),
            transposedUp.notes,
        )
    }

    @Test
    fun testAddNotesAllOk2() {
        val originalTune = simpleTune()
        val transposedDown: Tune = TransposedTune(originalTune, -1)
        transposedDown.addNote(Note(49, 2.0))
        transposedDown.addNote(Note(59, 4.0))
        transposedDown.addNote(Note(69, 8.0))
        transposedDown.addNote(Note(79, 12.0))
        assertEquals(longerTuneNotes(), originalTune.notes)
        assertEquals(
            listOf(
                Note(0, 2.0),
                Note(9, 4.0),
                Note(19, 8.0),
                Note(39, 12.0),
                Note(49, 2.0),
                Note(59, 4.0),
                Note(69, 8.0),
                Note(79, 12.0),
            ),
            transposedDown.notes,
        )
    }

    @Test
    fun testAddNotesSomeTooHigh() {
        val originalTune = simpleTune()
        val transposedDown: Tune = TransposedTune(originalTune, -10)
        transposedDown.addNote(Note(189, 2.0))
        transposedDown.addNote(Note(190, 4.0))
        transposedDown.addNote(Note(191, 8.0))
        transposedDown.addNote(Note(192, 12.0))
        assertEquals(
            listOf(
                Note(1, 2.0),
                Note(10, 4.0),
                Note(20, 8.0),
                Note(40, 12.0),
                Note(199, 2.0),
                Note(200, 4.0),
                Note(200, 8.0),
                Note(200, 12.0),
            ),
            originalTune.notes,
        )
        assertEquals(
            listOf(
                Note(0, 2.0),
                Note(0, 4.0),
                Note(10, 8.0),
                Note(30, 12.0),
                Note(189, 2.0),
                Note(190, 4.0),
                Note(190, 8.0),
                Note(190, 12.0),
            ),
            transposedDown.notes,
        )
    }

    @Test
    fun testAddNotesSomeTooLow() {
        val originalTune = simpleTune()
        val transposedUp: Tune = TransposedTune(originalTune, 10)
        transposedUp.addNote(Note(11, 2.0))
        transposedUp.addNote(Note(10, 4.0))
        transposedUp.addNote(Note(9, 8.0))
        transposedUp.addNote(Note(8, 12.0))
        assertEquals(
            listOf(
                Note(1, 2.0),
                Note(10, 4.0),
                Note(20, 8.0),
                Note(40, 12.0),
                Note(1, 2.0),
                Note(0, 4.0),
                Note(0, 8.0),
                Note(0, 12.0),
            ),
            originalTune.notes,
        )
        assertEquals(
            listOf(
                Note(11, 2.0),
                Note(20, 4.0),
                Note(30, 8.0),
                Note(50, 12.0),
                Note(11, 2.0),
                Note(10, 4.0),
                Note(10, 8.0),
                Note(10, 12.0),
            ),
            transposedUp.notes,
        )
    }

    @Test
    fun testIterator() {
        val targetTune = StandardTune()
        targetTune.addNote(Note(10, 1.0))

        val transposedTune = TransposedTune(targetTune, 2)
        val iteratedNotes = mutableListOf<Note>()

        // This perfectly tests your operator fun iterator()
        for (note in transposedTune) {
            iteratedNotes.add(note)
        }

        val expectedNotes = listOf(Note(12, 1.0))
        assertEquals(expectedNotes, iteratedNotes)
    }
}
