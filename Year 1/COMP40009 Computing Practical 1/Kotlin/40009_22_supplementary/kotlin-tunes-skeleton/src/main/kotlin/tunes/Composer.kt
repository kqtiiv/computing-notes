package tunes

class Composer(
    private val notes: List<Note>,
    private val targetTune: Tune,
) : Runnable {
    override fun run() {
        notes.forEach { targetTune.addNote(it) }
    }
}
