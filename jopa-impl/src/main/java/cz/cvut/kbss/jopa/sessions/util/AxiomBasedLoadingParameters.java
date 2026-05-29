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
package cz.cvut.kbss.jopa.sessions.util;

import cz.cvut.kbss.ontodriver.model.Axiom;

import java.util.Collection;

/**
 * Loading parameters for loading an entity from axioms.
 *
 * @param cls    Entity class
 * @param axioms Axioms to load the entity from
 * @param config Additional config for loading entity based on axioms
 * @param <T>    Entity type
 */
public record AxiomBasedLoadingParameters<T>(Class<T> cls, Collection<Axiom<?>> axioms,
                                             AxiomBasedLoadingConfigGroup<T> config) {

    /**
     * Returns {@code true} to indicate that the cache should always be bypassed when loading entity directly from
     * provided axioms.
     *
     * @return {@code true}
     */
    public boolean bypassCache() {
        return true;
    }
}
