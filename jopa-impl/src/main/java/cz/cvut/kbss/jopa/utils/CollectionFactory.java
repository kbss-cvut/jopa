package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.adapters.IndirectList;
import cz.cvut.kbss.jopa.adapters.IndirectMap;
import cz.cvut.kbss.jopa.adapters.IndirectSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

import java.lang.reflect.Field;
import java.util.*;

public final class CollectionFactory {

    private final UnitOfWorkImpl uow;

    public CollectionFactory(UnitOfWorkImpl uow) {
        this.uow = uow;
    }

    /**
     * Creates an indirect collection which wraps the specified collection and propagates changes to the current
     * persistence context.
     *
     * @param collection The collection to be proxied
     * @param owner      Collection owner instance
     * @param field      Field whose value the collection is
     * @return Indirect collection wrapping the collection
     */
    public IndirectCollection<?> createIndirectCollection(Object collection, Object owner, Field field) {
        final IndirectCollection<?> res;
        if (collection instanceof List) {
            res = new IndirectList<>(owner, field, uow, (List<?>) collection);
        } else if (collection instanceof Set) {
            res = new IndirectSet<>(owner, field, uow, (Set<?>) collection);
        } else if (collection instanceof Map) {
            res = new IndirectMap<>(owner, field, uow, (Map<?, ?>) collection);
        } else {
            throw new UnsupportedOperationException("Unsupported collection type " + collection.getClass());
        }
        return res;
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
