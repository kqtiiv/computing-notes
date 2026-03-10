package textfiles

interface TextFile {
    /**
     * Provides the length of the text file.
     *
     * @return the number of characters in the text file
     */
    val length: Int

    /**
     * Inserts text into the file.
     *
     * @param offset   The offset at which the text should be inserted
     * @param toInsert The text to be inserted
     * @throws FileIndexOutOfBoundsException if offset is negative or larger
     *  than the length of the file
     */
    fun insertText(offset: Int, toInsert: String)

    /**
     * Deletes text from the file.
     *
     * @param offset The offset at which the deletion should occur
     * @param size   The number of characters to be deleted
     * @throws FileIndexOutOfBoundsException if offset or size is negative, or
     *  if offset + size is larger than the length of the file
     */
    fun deleteText(offset: Int, size: Int)
}
