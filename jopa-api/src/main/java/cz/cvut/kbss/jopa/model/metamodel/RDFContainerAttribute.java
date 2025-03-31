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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.NonJPA;
import cz.cvut.kbss.jopa.model.annotations.RDFContainerType;

/**
 * Instances of the type {@code RDFContainerAttribute} represent attributes mapped to RDF containers.
 * <p>
 * RDF containers may be represented by different collections, depending on the type of the container. While a
 * {@literal rdf:Seq} is likely to be represented by a {@link java.util.List}, as it allows duplicates but is ordered, a
 * {@literal rdf:Alt} represents a set of alternatives and will thus probably be represented by a {@link java.util.Set},
 * possibly an implementation preserving order. A {@literal rdf:Bag} allows duplicates and is unordered, but will likely
 * be represented also by a {@link java.util.List}.
 *
 * @param <X> The type the represented collection belongs to
 * @param <C> Type of the collection
 * @param <E> The element type of the represented collection
 */
@NonJPA
public interface RDFContainerAttribute<X, C, E> extends PluralAttribute<X, C, E> {

    /**
     * Type of the RDF container represented by this attribute.
     *
     * @return RDF container type
     */
    @NonJPA
    RDFContainerType getContainerType();

    @Override
    default boolean isRdfContainer() {
        return true;
    }
}
