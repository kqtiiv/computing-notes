package proglang

import java.util.concurrent.locks.Lock
import kotlin.concurrent.withLock

class ProgramExecutor(
    private val threadBody: Stmt,
    private val pauseValue: Long,
    private val lock: Lock,
    private val store: MutableMap<String, Int>,
) : Runnable {
    override fun run() {
        var curStmt: Stmt? = threadBody.clone()
        while (curStmt != null) {
            Thread.sleep(pauseValue)
            lock.withLock {
                curStmt = curStmt?.step(store)
            }
        }
    }
}
