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

public abstract class OneLevelCascadeExplorer {

    protected CascadeType ct;

    public void start(final AbstractEntityManager pc, final Object o, CascadeType ct) {
        this.ct = ct;

        final EntityType<?> a = pc.getMetamodel().entity(o.getClass());
        for (final Attribute<?, ?> at : a.getAttributes()) {

            final List<CascadeType> cTypes = Arrays.asList(at.getCascadeTypes());

            try {
                if (!cTypes.contains(CascadeType.ALL) && !cTypes.contains(this.ct)) {
                    exploreNonCascaded(at, o);
                } else {
                    exploreCascaded(at, o);
                }
            } catch (Exception e) {
                throw new OWLPersistenceException(e);
            }
        }
    }

    protected void exploreCascaded(final Attribute<?, ?> at, final Object o) {
        // empty body
    }

    protected void exploreNonCascaded(final Attribute<?, ?> at, final Object o) {
        // empty body
    }
}
