package tunes

// import java.util.Collections

class StandardTune : Tune {
    private val _notes: MutableList<Note> = mutableListOf()

    override val notes: List<Note>
//        get() = Collections.unmodifiableList(_notes)
        get() = _notes.toList()

    override fun addNote(note: Note) {
        _notes.add(note)
    }
}
