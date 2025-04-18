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
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;

/**
 * Instances of the type ListAttribute represent persistent {@link java.util.List}-valued attributes.
 *
 * @param <X> The type the represented List belongs to
 * @param <E> The element type of the represented List
 */
public interface ListAttribute<X, E> extends PluralAttribute<X, java.util.List<E>, E> {

    /**
     * Gets the type of the sequence.
     *
     * @return List type
     */
    @NonJPA
    SequenceType getSequenceType();

    /**
     * Gets the IRI of the class that represents the 'OWLList' concept.
     * <p>
     * This is relevant only for referenced lists.
     *
     * @return List type IRI
     */
    @NonJPA
    IRI getListClassIRI();

    /**
     * Gets IRI of the property representing the relation between a list node and its content (value).
     * <p>
     * Relevant only for referenced lists.
     *
     * @return Property IRI
     */
    @NonJPA
    IRI getHasContentsPropertyIRI();

    /**
     * Gets IRI of the property representing next node in the list.
     *
     * @return Property IRI
     */
    @NonJPA
    IRI getHasNextPropertyIRI();

    /**
     * Whether this list represents a <a href="https://www.w3.org/TR/rdf12-schema/#ch_collectionvocab">RDF
     * collection</a>.
     *
     * @return {@code true} when this list attribute is an RDF collection, {@code false} otherwise
     * @see cz.cvut.kbss.jopa.model.annotations.RDFCollection
     */
    @NonJPA
    default boolean isRDFCollection() { return false; }
}
