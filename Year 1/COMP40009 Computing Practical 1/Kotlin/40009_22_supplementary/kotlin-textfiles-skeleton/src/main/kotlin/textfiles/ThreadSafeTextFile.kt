package textfiles

import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

class ThreadSafeTextFile(
    private val target: TextFile,
) : TextFile {
    private val lock: Lock = ReentrantLock()

    override val length: Int
        get() = lock.withLock { target.length }

    override fun deleteText(
        offset: Int,
        size: Int,
    ) = lock.withLock { target.deleteText(offset, size) }

    override fun insertText(
        offset: Int,
        toInsert: String,
    ) = lock.withLock { target.insertText(offset, toInsert) }

    override fun toString(): String = lock.withLock { target.toString() }
}
