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
 * Specifies mapping of an <a href="https://www.w3.org/TR/rdf12-schema/#ch_containervocab">RDF container</a>.
 * <p>
 * RDF containers are resources used to represent collections. In contrast to RDF collections, they are not closed, so
 * it is possible to add new items to them without having to reattach the container ending element.
 * <p>
 * Three types of RDF containers are defined, each with different convention-based semantics.
 *
 * @see RDFContainerType
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD, ElementType.METHOD})
public @interface RDFContainer {

    /**
     * The type of the container.
     *
     * @return Type of the container
     */
    RDFContainerType type();
}
