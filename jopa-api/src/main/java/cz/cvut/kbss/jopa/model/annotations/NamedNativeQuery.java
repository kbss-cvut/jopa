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
package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies a named native SPARQL query.
 * <p>
 * Query names are scoped to the persistence unit. The NamedNativeQuery annotation can be applied to an entity or mapped
 * superclass.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface NamedNativeQuery {

    /**
     * The name used to refer to the query with the {@link cz.cvut.kbss.jopa.model.EntityManager} methods that create
     * query objects.
     *
     * @return Name of the query
     */
    String name();

    /**
     * The SPARQL query string.
     *
     * @return The query
     */
    String query();
}
