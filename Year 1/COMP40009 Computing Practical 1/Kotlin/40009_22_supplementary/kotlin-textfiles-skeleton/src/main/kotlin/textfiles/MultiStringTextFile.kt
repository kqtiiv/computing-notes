package textfiles

private const val BLOCK_SIZE: Int = 8

class MultiStringTextFile(initialContents: String) : TextFile {

    private var blocks: MutableList<StringBuilder> = mutableListOf(StringBuilder(initialContents))

    /**
     * Yields the number of blocks that are currently used to represent the file.
     *
     * @return The number of blocks used to represent this file.
     */
    val numBlocks: Int
        get() = blocks.size

    override val length: Int
        get() = TODO("To be implemented.")

    init {
        rebalance()
    }

    /**
     * Reorganises the internal representation of the text file so that the content is partitioned
     * into blocks of equal size, except that the final block may be shorter.
     */
    fun rebalance() {
        TODO("To be implemented.")
    }

    override fun insertText(offset: Int, toInsert: String) {
        TODO("To be implemented.")
    }

    override fun deleteText(offset: Int, size: Int) {
        if (offset < 0 || size < 0) {
            throw FileIndexOutOfBoundsException()
        }
        val newBlocks: MutableList<StringBuilder> = mutableListOf()
        var index = 0
        var block = 0
        while (block < blocks.size) {
            if (index < offset + size && offset < index + blocks[block].length) {
                if (size == 0) {
                    return
                }
                // Add the prefix
                val prefixSize: Int = offset - index
                if (prefixSize > 0) {
                    newBlocks.add(StringBuilder(blocks[block].substring(0, prefixSize)))
                }
                index += prefixSize
                // If the suffix is in the same block, add it. Otherwise, skip over blocks.
                if (prefixSize + size < blocks[block].length) {
                    newBlocks.add(StringBuilder(blocks[block].substring(prefixSize + size)))
                } else {
                    var deleted: Int = blocks[block].length - prefixSize
                    block++
                    while (block < blocks.size && size - deleted >= blocks[block].length) {
                        deleted += blocks[block].length
                        block++
                    }
                    if (deleted < size) {
                        if (block >= blocks.size) {
                            throw FileIndexOutOfBoundsException()
                        }
                        assert(blocks[block].length > size - deleted)
                        newBlocks.add(StringBuilder(blocks[block].substring(size - deleted)))
                    }
                }
                index += size
            } else {
                newBlocks.add(blocks[block])
                index += blocks[block].length
            }
            block++
        }
        if (offset >= index) {
            if (offset > index || size > 0) {
                throw FileIndexOutOfBoundsException()
            }
        }
        blocks = newBlocks
    }
}
