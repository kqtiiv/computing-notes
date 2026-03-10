package tunes;

import org.jetbrains.annotations.NotNull;

import java.util.Iterator;
import java.util.List;

import static tunes.NoteUtilsKt.MAX_DURATION;

public class StretchedTune implements Tune {
    private final Tune targetTune;
    private final double stretchFactor;

    public StretchedTune(Tune targetTune, double stretchFactor) {
        this.targetTune = targetTune;
        this.stretchFactor = stretchFactor;
    }

    private Note rescale(Note note) {
        return new Note(note.getPitch(), Math.min(note.getDuration() * stretchFactor, MAX_DURATION));
    }

    @NotNull
    @Override
    public List<Note> getNotes() {
        return targetTune.getNotes().stream().map(this::rescale).toList();
    }

    @Override
    public void addNote(@NotNull Note note) {
        targetTune.addNote(new Note(note.getPitch(), Math.min(note.getDuration() / stretchFactor, MAX_DURATION)));
    }

    @NotNull
    @Override
    public Iterator<Note> iterator() {
        return getNotes().iterator();
    }

    @Override
    public double getTotalDuration() {
        return getNotes().stream().mapToDouble(Note::getDuration).sum();
    }
}
