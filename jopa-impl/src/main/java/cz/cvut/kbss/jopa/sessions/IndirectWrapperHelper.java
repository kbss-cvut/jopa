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

class IndirectWrapperHelper {

    private final UnitOfWorkImpl uow;

    IndirectWrapperHelper(UnitOfWorkImpl uow) {
        this.uow = uow;
    }

    public Object createIndirectWrapper(Object wrapped, Object owner, Field field) {
        final Object res;
        if (wrapped instanceof List) {
            res = new IndirectList<>(owner, field, uow, (List<?>) wrapped);
        } else if (wrapped instanceof Set) {
            res = new IndirectSet<>(owner, field, uow, (Set<?>) wrapped);
        } else if (wrapped instanceof Map) {
            res = new IndirectMap<>(owner, field, uow, (Map<?, ?>) wrapped);
        } else if (wrapped instanceof MultilingualString) {
            res = new IndirectMultilingualString(owner, field, uow, (MultilingualString) wrapped);
        } else {
            throw new UnsupportedOperationException("Unsupported wrapped type " + wrapped.getClass());
        }
        return res;
    }

    static boolean requiresIndirectWrapper(Object target) {
        return target instanceof Collection || target instanceof MultilingualString || target instanceof Map;
    }
}
