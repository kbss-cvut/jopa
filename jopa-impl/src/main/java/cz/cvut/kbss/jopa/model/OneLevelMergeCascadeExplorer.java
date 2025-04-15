/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;

import java.util.Arrays;
import java.util.List;

/**
 * Merge cascade resolver.
 * <p>
 * Cascades the merge operation to attributes which support merge cascading.
 */
class OneLevelMergeCascadeExplorer {

    void start(final AbstractEntityManager pc, final Object merged, final Object toMerge) {

        final EntityType<?> a = pc.getMetamodel().entity(toMerge.getClass());
        for (final Attribute<?, ?> at : a.getAttributes()) {

            final List<CascadeType> cTypes = Arrays.asList(at.getCascadeTypes());

            try {
                if (cTypes.contains(CascadeType.ALL) || cTypes.contains(CascadeType.MERGE)) {
                    exploreCascaded(at, merged, toMerge);
                }
            } catch (Exception e) {
                throw new OWLPersistenceException(e);
            }
        }
    }

    void exploreCascaded(final Attribute<?, ?> at, final Object merged, final Object toMerge) {
        // empty body
    }
}
