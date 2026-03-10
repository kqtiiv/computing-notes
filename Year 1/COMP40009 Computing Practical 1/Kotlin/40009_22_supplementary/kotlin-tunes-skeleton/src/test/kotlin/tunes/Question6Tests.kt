package tunes

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertSame
import kotlin.test.fail

class Question6Tests {
    private val tune1: Tune = StandardTune()
    private val tune2: Tune = StandardTune()
    private val tune3: Tune = StandardTune()
    private val tune4: Tune = StandardTune()
    private val tune5: Tune = StandardTune()
    private val tune6: Tune = StandardTune()
    private val tune7: Tune = StandardTune()
    private val tune8: Tune = StandardTune()
    private val tune9: Tune = StandardTune()
    private val tune10: Tune = StandardTune()

    init {
        tune1.addNote(Note(12, 4.0))
        tune2.addNote(Note(6, 8.0))
        tune3.addNote(Note(18, 4.0))
        tune3.addNote(Note(19, 4.0))
        tune4.addNote(Note(180, 0.25))
        tune4.addNote(Note(170, 0.25))
        tune4.addNote(Note(160, 0.25))
        tune5.addNote(Note(16, 0.25))
        tune5.addNote(Note(60, 0.25))
        tune5.addNote(Note(163, 0.5))
        tune7.addNote(Note(1, 1.0))
        tune7.addNote(Note(1, 1.0))
        tune7.addNote(Note(1, 1.0))
        tune7.addNote(Note(1, 1.0))
        tune7.addNote(Note(1, 1.0))
        tune8.addNote(Note(12, 10.0))
        tune8.addNote(Note(14, 1.0))
        tune9.addNote(Note(140, 1.0))
        tune9.addNote(Note(140, 1.0))
        tune9.addNote(Note(141, 1.0))
        tune9.addNote(Note(142, 1.0))
        tune10.addNote(Note(1, 1.0))
        tune10.addNote(Note(1, 1.0))
        tune10.addNote(Note(1, 1.0))
    }

    @Test
    fun testEmptySongCollection() {
        val songCollection = SongCollection()
        assertEquals(emptyList(), songCollection.getSongNames())
    }

    @Test
    fun testSmallCollection() {
        val songCollection = SongCollection()
        songCollection.addSong("Carol", tune1)
        songCollection.addSong("Anthem", tune2)
        songCollection.addSong("Ballad", tune3)
        assertEquals(listOf("Anthem", "Ballad", "Carol"), songCollection.getSongNames())
        assertSame(tune1, songCollection.getTune("Carol"))
        assertSame(tune2, songCollection.getTune("Anthem"))
        assertSame(tune3, songCollection.getTune("Ballad"))
        try {
            songCollection.getTune("Rocker")
            fail("Expected NoSuchElementException")
        } catch (exception: NoSuchElementException) {
            // Good; an exception was expected.
        }
        try {
            songCollection.addSong("Carol", tune4)
            fail("Expected UnsupportedOperationException")
        } catch (exception: UnsupportedOperationException) {
            // Good; an exception was expected.
        }
    }

    @Test
    fun testLargeCollection() {
        val songCollection = SongCollection()
        songCollection.addSong("Killer Queen", tune1)
        songCollection.addSong("Another One Bites The Dust", tune2)
        songCollection.addSong("Innuendo", tune3)
        songCollection.addSong("I Want It All", tune4)
        assertSame(tune1, songCollection.getTune("Killer Queen"))
        assertSame(tune2, songCollection.getTune("Another One Bites The Dust"))
        assertSame(tune3, songCollection.getTune("Innuendo"))
        assertSame(tune4, songCollection.getTune("I Want It All"))
        assertEquals(listOf("Another One Bites The Dust", "I Want It All", "Innuendo", "Killer Queen"), songCollection.getSongNames())

        songCollection.addSong("Headlong", tune5)
        songCollection.addSong("Don't Try So Hard", tune6)
        songCollection.addSong("Tenement Funster", tune7)
        songCollection.addSong("It's A Kind Of Magic", tune8)
        songCollection.addSong("Breakthru", tune9)
        songCollection.addSong("Who Wants To Live Forever?", tune10)
        assertSame(tune1, songCollection.getTune("Killer Queen"))
        assertSame(tune2, songCollection.getTune("Another One Bites The Dust"))
        assertSame(tune3, songCollection.getTune("Innuendo"))
        assertSame(tune4, songCollection.getTune("I Want It All"))
        assertSame(tune5, songCollection.getTune("Headlong"))
        assertSame(tune6, songCollection.getTune("Don't Try So Hard"))
        assertSame(tune7, songCollection.getTune("Tenement Funster"))
        assertSame(tune8, songCollection.getTune("It's A Kind Of Magic"))
        assertSame(tune9, songCollection.getTune("Breakthru"))
        assertSame(tune10, songCollection.getTune("Who Wants To Live Forever?"))
        assertEquals(
            listOf(
                "Another One Bites The Dust",
                "Breakthru",
                "Don't Try So Hard",
                "Headlong",
                "I Want It All",
                "Innuendo",
                "It's A Kind Of Magic",
                "Killer Queen",
                "Tenement Funster",
                "Who Wants To Live Forever?",
            ),
            songCollection.getSongNames(),
        )
    }

    @Test
    fun testGetTuneEmptyCollection() {
        val songCollection = SongCollection()
        try {
            songCollection.getTune("Anything")
            fail("Expected NoSuchElementException on empty collection")
        } catch (exception: NoSuchElementException) {
            // Good
        }
    }
}
