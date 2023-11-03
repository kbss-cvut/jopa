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
package cz.cvut.kbss.jopa.model;

import java.net.URI;
import java.net.URL;
import java.util.Set;

public class PersistenceProperties {

    /**
     * Persistence provider class configuration parameter.
     * <p>
     * This is the configuration parameter whose value is used by {@link cz.cvut.kbss.jopa.Persistence} to instantiate
     * the persistence provider for a persistence unit.
     */
    public static final String JPA_PERSISTENCE_PROVIDER = "cz.cvut.kbss.jopa.model.PersistenceProvider";

    /**
     * Supported identifier Java types.
     * <p>
     * This set represents the Java types that can be used as entity identifiers or as {@link
     * cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty} property values.
     */
    public static final Set<Class<?>> IDENTIFIER_TYPES = Set.of(URI.class, URL.class, String.class);

    PersistenceProperties() {
        throw new AssertionError();
    }
}
