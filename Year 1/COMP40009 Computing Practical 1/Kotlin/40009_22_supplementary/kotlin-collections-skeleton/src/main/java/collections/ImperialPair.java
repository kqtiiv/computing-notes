package collections;

public class ImperialPair<S, T> {
    private final S first;
    private final T second;

    public ImperialPair(S first, T second) {
        this.first = first;
        this.second = second;
    }

    public S getFirst() {
        return first;
    }

    public T getSecond() {
        return second;
    }
}
