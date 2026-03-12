package textfiles

import java.util.Random

class Author(
    private val strings: List<String>,
    private val target: TextFile,
    private val numGenerator: Random,
) : Runnable {
    override fun run() {
        strings.forEach { target.insertText(numGenerator.nextInt(target.length + 1), it) }
    }
}
