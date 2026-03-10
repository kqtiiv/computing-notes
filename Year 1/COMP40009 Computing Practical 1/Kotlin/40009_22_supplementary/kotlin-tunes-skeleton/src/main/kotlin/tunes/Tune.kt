package tunes

interface Tune {
    val notes: List<Note>
    val totalDuration: Double
        get() = notes.sumOf { it.duration }

    fun addNote(note: Note)

    operator fun iterator(): Iterator<Note> = notes.iterator()
}
