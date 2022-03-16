package cz.cvut.kbss.jopa.datatype.util;

import java.util.AbstractMap;

/**
 * Represents an ordered pair of values.
 *
 * @param <F> Type of the first value in the pair
 * @param <S> Type of the second value in the pair
 */
public final class Pair<F, S> extends AbstractMap.SimpleEntry<F, S> {

    public Pair(F first, S second) {
        super(first, second);
    }

    public F getFirst() {
        return getKey();
    }

    public S getSecond() {
        return getValue();
    }

    @Override
    public String toString() {
        return "(" + getFirst() + ", " + getSecond() + ")";
    }
}
