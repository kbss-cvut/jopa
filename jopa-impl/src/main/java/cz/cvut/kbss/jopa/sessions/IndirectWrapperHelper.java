/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.adapters.change.ChangeTrackingIndirectList;
import cz.cvut.kbss.jopa.adapters.change.ChangeTrackingIndirectMap;
import cz.cvut.kbss.jopa.adapters.change.ChangeTrackingIndirectMultilingualString;
import cz.cvut.kbss.jopa.adapters.change.ChangeTrackingIndirectSet;
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
        if (wrapped instanceof List) {
            return new ChangeTrackingIndirectList<>(owner, field, uow, (List<?>) wrapped);
        } else if (wrapped instanceof Set) {
            return new ChangeTrackingIndirectSet<>(owner, field, uow, (Set<?>) wrapped);
        } else if (wrapped instanceof Map) {
            return new ChangeTrackingIndirectMap<>(owner, field, uow, (Map<?, ?>) wrapped);
        } else if (wrapped instanceof MultilingualString) {
            return new ChangeTrackingIndirectMultilingualString(owner, field, uow, (MultilingualString) wrapped);
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
