package tunes

import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock
import kotlin.test.Test
import kotlin.test.assertEquals

private class ExceptionMonitoringThread(
    private val exceptions: MutableList<Exception>,
    private val lock: Lock,
    private var body: () -> Unit,
) : Runnable {
    override fun run() {
        try {
            body()
        } catch (e: Exception) {
            lock.withLock { exceptions.add(e) }
        }
    }
}

class Question5Tests {
    @Test
    fun concurrencyTest() {
        for (repeat in 1..20) {
            println("Repeat run $repeat")
            val composerNotes: List<MutableList<Note>> = (0..<8).map { mutableListOf() }
            val expectedNotes: MutableSet<Note> = mutableSetOf()
            for (i in 0..<20000) {
                (0..<8).forEach {
                    val note = Note(i % 200, (i % 4 * it + 1).toDouble())
                    composerNotes[it].add(note)
                    expectedNotes.add(note)
                }
            }

            // TODO: create a ThreadSafeTune
            val tune = ThreadSafeTune(StandardTune())

            // TODO: create eight Composers using the composerNotes lists
            val composerBodies = composerNotes.map { Composer(it, tune) }

            // TODO: create eight Threads, one per Composer
            val threads = composerBodies.map { Thread(it) }

            // TODO: start the threads
            threads.forEach(Thread::start)

            // TODO: join the threads
            threads.forEach(Thread::join)

            assertEquals(expectedNotes, tune.notes.toSet())
        }
    }

    @Test
    fun safeIterationTest() {
        val tune = ThreadSafeTune(StandardTune())
        tune.addNote(Note(64, 1.0))
        val iterator = tune.iterator()
        tune.addNote(Note(64, 1.0))
        iterator.next()
    }

    @Test
    fun durationTest() {
        val tune = ThreadSafeTune(StandardTune())
        val exceptions = mutableListOf<Exception>()
        val lock = ReentrantLock()

        val adder =
            Thread(
                ExceptionMonitoringThread(exceptions, lock) {
                    for (i in 1..1000) tune.addNote(Note(60, 1.0))
                },
            )

        val duration =
            Thread(
                ExceptionMonitoringThread(exceptions, lock) {
                    for (i in 1..1000) tune.totalDuration
                },
            )

        adder.start()
        duration.start()

        adder.join()
        duration.join()

        assertEquals(0, exceptions.size)
        assertEquals(1000.0, tune.totalDuration)
    }
}
