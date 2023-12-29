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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.NonJPA;
import cz.cvut.kbss.jopa.model.IRI;

/**
 * Instances of the type EntityType represent entity types.
 *
 * @param <X> The represented entity type.
 */
public interface EntityType<X> extends Bindable<X>, IdentifiableType<X> {

    /**
     * Return the entity name.
     *
     * @return entity name
     */
    String getName();

    /**
     * Gets IRI of the OWL class represented by this entity type.
     *
     * @return OWL class IRI
     */
    @NonJPA
    IRI getIRI();
}
