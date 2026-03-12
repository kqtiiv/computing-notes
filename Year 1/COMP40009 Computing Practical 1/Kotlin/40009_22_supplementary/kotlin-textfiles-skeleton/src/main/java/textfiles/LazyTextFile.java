package textfiles;

import org.jetbrains.annotations.NotNull;

public final class LazyTextFile implements TextFile {
    private final TextFile target;

    public LazyTextFile(TextFile target) {
        this.target = target;
    }

    private boolean isPending = false;

    private int insertOffset = 0;

    private StringBuilder insertString = new StringBuilder();

    @Override
    public int getLength() {
        flush();
        return target.getLength();
    }

    @Override
    public void deleteText(int offset, int size) {
        flush();
        target.deleteText(offset, size);
    }

    @Override
    public void insertText(int offset, @NotNull String toInsert) {
        if (isPending && insertOffset == offset) {
            insertString.insert(0, toInsert);
        } else {
            flush();
            isPending = true;
            insertOffset = offset;
            insertString = new StringBuilder(toInsert);
        }
    }

    @Override
    public String toString() {
        flush();
        return target.toString();
    }

    @Override
    public int compareTo(@NotNull TextFile other) {
        return TextFile.super.toString().compareTo(other.toString());
    }

    private void flush() {
        if (isPending) {
            target.insertText(insertOffset, insertString.toString());
            isPending = false;
        }
    }
}
