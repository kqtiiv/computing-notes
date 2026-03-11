package proglang

import java.util.concurrent.locks.ReentrantLock

class ConcurrentProgram(
    private val threadBodies: List<Stmt>,
    private val pauseValues: List<Long>,
) {
    init {
        require(threadBodies.size == pauseValues.size)
    }

    private val lock = ReentrantLock()

    fun execute(store: Map<String, Int>): Map<String, Int> {
        val workingStore: MutableMap<String, Int> = store.toMutableMap()
        for (storeEntryKey: String in store.keys) {
            workingStore[storeEntryKey] = store[storeEntryKey]!!
        }
        val threadList: List<Thread> =
            List(threadBodies.size) {
                Thread(
                    ProgramExecutor(
                        threadBodies[it],
                        pauseValues[it],
                        lock,
                        workingStore,
                    ),
                )
            }
        threadList.forEach { it.start() }
        threadList.forEach { it.join() }
        return workingStore
    }
}
