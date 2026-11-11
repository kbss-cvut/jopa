/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.proxy.change.ChangeTrackingIndirectList;
import cz.cvut.kbss.jopa.proxy.change.ChangeTrackingIndirectMap;
import cz.cvut.kbss.jopa.proxy.change.ChangeTrackingIndirectMultilingualString;
import cz.cvut.kbss.jopa.proxy.change.ChangeTrackingIndirectSet;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;

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

    private final UnitOfWork uow;

    IndirectWrapperHelper(UnitOfWork uow) {
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
        return switch (wrapped) {
            case List list -> new ChangeTrackingIndirectList<>(owner, field, uow, list);
            case Set set -> new ChangeTrackingIndirectSet<>(owner, field, uow, set);
            case MultilingualString multilingualString ->
                // MultilingualString check has to precede the Map check since it is a Map implementation
                    new ChangeTrackingIndirectMultilingualString(owner, field, uow, multilingualString);
            case Map map -> new ChangeTrackingIndirectMap<>(owner, field, uow, map);
            default -> throw new UnsupportedOperationException("Unsupported wrapped type " + wrapped.getClass());
        };
    }

    /**
     * Checks whether the specified object is of a type requiring an indirect wrapper.
     *
     * @param target Target object to check
     * @return {@code true} if an indirect wrapper is used for the specified target
     */
    static boolean requiresIndirectWrapper(Object target) {
        return (!(target instanceof LazyLoadingProxy)) && (target instanceof Collection || target instanceof Map);
    }
}
