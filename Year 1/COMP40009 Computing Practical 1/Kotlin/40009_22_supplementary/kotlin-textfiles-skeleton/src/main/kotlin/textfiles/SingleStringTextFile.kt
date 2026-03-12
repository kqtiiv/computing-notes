package textfiles

class SingleStringTextFile(
    initContent: String,
) : TextFile {
    private val contents: StringBuilder = StringBuilder(initContent)

    override val length: Int
        get() = contents.length

    override fun insertText(
        offset: Int,
        toInsert: String,
    ) {
        if (offset in 0..length) {
            contents.insert(offset, toInsert)
        } else {
            throw FileIndexOutOfBoundsException()
        }
    }

    override fun deleteText(
        offset: Int,
        size: Int,
    ) {
        if (offset < 0 || size < 0 || offset + size > length) throw FileIndexOutOfBoundsException()
        contents.delete(offset, offset + size)
    }

    override fun toString(): String = contents.toString()
}
