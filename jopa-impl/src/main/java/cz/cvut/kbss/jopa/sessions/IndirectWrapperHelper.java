package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectList;
import cz.cvut.kbss.jopa.adapters.IndirectMap;
import cz.cvut.kbss.jopa.adapters.IndirectMultilingualString;
import cz.cvut.kbss.jopa.adapters.IndirectSet;
import cz.cvut.kbss.jopa.model.MultilingualString;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Helper for dealing with indirect wrappers (mainly collections, multilingual strings).
 * <p>
 * Indirect wrappers are used to ensure that changes to the wrapped instances are propagated into the persistence
 * context during transactions.
 */
class IndirectWrapperHelper {

    private final UnitOfWorkImpl uow;

    IndirectWrapperHelper(UnitOfWorkImpl uow) {
        this.uow = uow;
    }

    /**
     * Creates an indirect wrapper for the specified {@code wrapped} object.
     *
     * @param wrapped The object to wrap
     * @param owner   Instance owning the wrapped object. Necessary to associate the changes with the correct subject
     * @param field   Field filled with the wrapped object
     * @return A suitable indirect wrapper
     */
    Object createIndirectWrapper(Object wrapped, Object owner, Field field) {
        assert requiresIndirectWrapper(wrapped);
        if (wrapped instanceof List) {
            return new IndirectList<>(owner, field, uow, (List<?>) wrapped);
        } else if (wrapped instanceof Set) {
            return new IndirectSet<>(owner, field, uow, (Set<?>) wrapped);
        } else if (wrapped instanceof Map) {
            return new IndirectMap<>(owner, field, uow, (Map<?, ?>) wrapped);
        } else if (wrapped instanceof MultilingualString) {
            return new IndirectMultilingualString(owner, field, uow, (MultilingualString) wrapped);
        } else {
            throw new UnsupportedOperationException("Unsupported wrapped type " + wrapped.getClass());
        }
    }

    /**
     * Checks whether the specified object is of a type requiring an indirect wrapper.
     *
     * @param target Target object to check
     * @return {@code true} if an indirect wrapper is used for the specified target
     */
    static boolean requiresIndirectWrapper(Object target) {
        return target instanceof Collection || target instanceof MultilingualString || target instanceof Map;
    }
}
