package cz.cvut.kbss.jopa.utils;

import java.util.*;

public final class CollectionFactory {

    private CollectionFactory() {
        throw new AssertionError();
    }

    /**
     * Creates an instance of a {@link Collection} implementation best matching the specified instance.
     * <p>
     * E.g. for any kind of {@link List}, an {@link ArrayList} is returned.
     *
     * @param collection Create matching instance for this collection
     * @return Best matching collection instance
     */
    public static Collection<?> createInstance(Collection<?> collection) {
        Objects.requireNonNull(collection);

        if (collection instanceof List) {
            return new ArrayList<>(collection.size());
        } else if (collection instanceof Set) {
            return new HashSet<>(collection.size());
        }
        throw new IllegalArgumentException("Unsupported collection type: " + collection);
    }
}
