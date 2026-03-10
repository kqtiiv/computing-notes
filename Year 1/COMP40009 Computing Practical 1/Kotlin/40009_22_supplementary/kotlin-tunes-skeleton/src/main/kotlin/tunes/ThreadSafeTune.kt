package tunes

import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

class ThreadSafeTune(
    private val targetTune: Tune,
) : Tune {
    private val lock = ReentrantLock()

    override fun addNote(note: Note) = lock.withLock { targetTune.addNote(note) }

    override fun iterator(): Iterator<Note> =
        lock.withLock {
            targetTune.iterator()
        }

    override val notes: List<Note>
        get() =
            lock.withLock {
                targetTune.notes
            }

    override val totalDuration: Double
        get() = lock.withLock { targetTune.totalDuration }
}
