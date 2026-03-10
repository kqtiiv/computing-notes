package tunes

private val notes: List<String> =
    listOf(
        "C",
        "C#",
        "D",
        "D#",
        "E",
        "F",
        "F#",
        "G",
        "G#",
        "A",
        "A#",
        "B",
    )

data class Note(
    val pitch: Int,
    val duration: Double,
) {
    init {
        require(pitch in 0..MAX_PITCH)
        require(duration > 0.0 && duration <= MAX_DURATION)
    }

    override fun toString(): String {
        val name: String = notes[pitch % notes.size]
        val octave: Int = pitch / notes.size
        return "$name$octave($duration)"
    }

    fun hasNoteAbove(): Boolean = pitch < MAX_PITCH

    fun hasNoteBelow(): Boolean = pitch > 0

    fun noteAbove(): Note = Note(pitch + 1, duration)

    fun noteBelow(): Note = Note(pitch - 1, duration)
}
