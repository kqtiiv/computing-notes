package tunes

import kotlin.test.Test
import kotlin.test.assertEquals

class Question4Tests {
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
    fun testStretchLonger() {
        val originalTune = simpleTune()
        val stretched: Tune = StretchedTune(originalTune, 2.0)
        assertEquals(simpleTuneNotes(), originalTune.notes)
        assertEquals(
            listOf(
                Note(1, 4.0),
                Note(10, 8.0),
                Note(20, 16.0),
                Note(40, 24.0),
            ),
            stretched.notes,
        )
    }

    @Test
    fun testStretchMuchLonger() {
        val originalTune = simpleTune()
        val stretched: Tune = StretchedTune(originalTune, 31.0)
        assertEquals(simpleTuneNotes(), originalTune.notes)
        assertEquals(
            listOf(
                Note(1, 62.0),
                Note(10, 64.0),
                Note(20, 64.0),
                Note(40, 64.0),
            ),
            stretched.notes,
        )
    }

    @Test
    fun testStretchShorter() {
        val originalTune = simpleTune()
        val stretched: Tune = StretchedTune(originalTune, 0.25)
        assertEquals(simpleTuneNotes(), originalTune.notes)
        assertEquals(
            listOf(
                Note(1, 0.5),
                Note(10, 1.0),
                Note(20, 2.0),
                Note(40, 3.0),
            ),
            stretched.notes,
        )
        assertEquals(6.5, stretched.totalDuration)
    }

    @Test
    fun testStretchMuchShorter() {
        val originalTune = simpleTune()
        val stretched: Tune = StretchedTune(originalTune, 1.0 / 32.0)
        assertEquals(simpleTuneNotes(), originalTune.notes)
        assertEquals(
            listOf(
                Note(1, 2.0 / 32.0),
                Note(10, 4.0 / 32.0),
                Note(20, 8.0 / 32.0),
                Note(40, 12.0 / 32.0),
            ),
            stretched.notes,
        )
        assertEquals(26.0 / 32.0, stretched.totalDuration)
    }

    @Test
    fun testStretchAndShrink() {
        val originalTune = simpleTune()
        val stretched: Tune = StretchedTune(originalTune, 2.0)
        val shrunk: Tune = StretchedTune(stretched, 0.5)
        assertEquals(simpleTuneNotes(), originalTune.notes)
        assertEquals(
            simpleTuneNotes(),
            shrunk.notes,
        )
    }

    @Test
    fun testAddNotesAllOk1() {
        val originalTune = simpleTune()
        val stretched: Tune = StretchedTune(originalTune, 2.0)
        stretched.addNote(Note(50, 4.0))
        stretched.addNote(Note(60, 8.0))
        stretched.addNote(Note(70, 16.0))
        stretched.addNote(Note(80, 24.0))
        assertEquals(longerTuneNotes(), originalTune.notes)
        assertEquals(
            listOf(
                Note(1, 4.0),
                Note(10, 8.0),
                Note(20, 16.0),
                Note(40, 24.0),
                Note(50, 4.0),
                Note(60, 8.0),
                Note(70, 16.0),
                Note(80, 24.0),
            ),
            stretched.notes,
        )
    }

    @Test
    fun testAddNotesAllOk2() {
        val originalTune = simpleTune()
        val stretched: Tune = StretchedTune(originalTune, 0.5)
        stretched.addNote(Note(50, 1.0))
        stretched.addNote(Note(60, 2.0))
        stretched.addNote(Note(70, 4.0))
        stretched.addNote(Note(80, 6.0))
        assertEquals(longerTuneNotes(), originalTune.notes)
        assertEquals(
            listOf(
                Note(1, 1.0),
                Note(10, 2.0),
                Note(20, 4.0),
                Note(40, 6.0),
                Note(50, 1.0),
                Note(60, 2.0),
                Note(70, 4.0),
                Note(80, 6.0),
            ),
            stretched.notes,
        )
    }

    @Test
    fun testAddNotesSomeTooSlow() {
        val originalTune = simpleTune()
        val stretched: Tune = StretchedTune(originalTune, 0.25)
        stretched.addNote(Note(189, 2.0))
        stretched.addNote(Note(190, 8.0))
        stretched.addNote(Note(191, 16.0))
        stretched.addNote(Note(192, 32.0))
        stretched.addNote(Note(193, 64.0))
        assertEquals(
            listOf(
                Note(1, 2.0),
                Note(10, 4.0),
                Note(20, 8.0),
                Note(40, 12.0),
                Note(189, 8.0),
                Note(190, 32.0),
                Note(191, 64.0),
                Note(192, 64.0),
                Note(193, 64.0),
            ),
            originalTune.notes,
        )
        assertEquals(
            listOf(
                Note(1, 0.5),
                Note(10, 1.0),
                Note(20, 2.0),
                Note(40, 3.0),
                Note(189, 2.0),
                Note(190, 8.0),
                Note(191, 16.0),
                Note(192, 16.0),
                Note(193, 16.0),
            ),
            stretched.notes,
        )
    }

    @Test
    fun testAddNotesAndIterate() {
        val originalTune = simpleTune()
        val stretched: Tune = StretchedTune(originalTune, 0.5)
        stretched.addNote(Note(50, 1.0))
        stretched.addNote(Note(60, 2.0))
        stretched.addNote(Note(70, 4.0))
        stretched.addNote(Note(80, 6.0))

        val copyTune = StandardTune()
        val copyStretched: Tune = StretchedTune(copyTune, 0.5)
        for (note in stretched) {
            copyStretched.addNote(Note(note.pitch, note.duration))
        }
        assertEquals(longerTuneNotes(), copyTune.notes)
        assertEquals(
            listOf(
                Note(1, 1.0),
                Note(10, 2.0),
                Note(20, 4.0),
                Note(40, 6.0),
                Note(50, 1.0),
                Note(60, 2.0),
                Note(70, 4.0),
                Note(80, 6.0),
            ),
            copyStretched.notes,
        )
    }
}
