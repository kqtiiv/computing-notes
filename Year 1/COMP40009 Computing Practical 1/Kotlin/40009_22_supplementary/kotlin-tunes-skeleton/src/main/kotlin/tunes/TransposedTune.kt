package tunes

class TransposedTune(
    private val targetTune: Tune,
    private val pitchOffset: Int,
) : Tune by targetTune {
    override val notes: List<Note>
        get() =
            targetTune.notes.map { note ->
                Note((note.pitch + pitchOffset).coerceIn(0, MAX_PITCH), note.duration)
            }

    override fun addNote(note: Note) {
        targetTune.addNote(Note((note.pitch - pitchOffset).coerceIn(0, MAX_PITCH), note.duration))
    }

    override fun iterator(): Iterator<Note> = notes.iterator()
}
