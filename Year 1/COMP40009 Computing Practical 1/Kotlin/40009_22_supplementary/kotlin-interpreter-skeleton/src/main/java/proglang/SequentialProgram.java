package proglang;

import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;

public final class SequentialProgram {
    private final Stmt topLevel;

    public SequentialProgram(Stmt topLevel) {
        this.topLevel = topLevel;
    }

    public Map<String, Integer> execute(Map<String, Integer> initialStore) {
        final Map<String, Integer> workingStore = new HashMap<>(initialStore);

        @Nullable
        Stmt curLine = topLevel;

        while (curLine != null) {
            curLine = StmtKt.step(curLine, workingStore);
        }

        return workingStore;
    }

    @Override
    public String toString() {
        return topLevel.toString();
    }
}
