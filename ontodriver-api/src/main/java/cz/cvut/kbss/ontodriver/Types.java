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
package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Collection;
import java.util.Set;

/**
 * This interface is used for working with individuals' types.
 */
public interface Types {

    /**
     * Gets types associated with the specified individual.
     *
     * @param individual      Resource for which types should be found
     * @param contexts         Context in which to look for the types. Empty collection indicates the default context
     * @param includeInferred Whether to include inferred types as well
     * @return Set of type URIs
     * @throws OntoDriverException When an ontology access error occurs
     */
    Set<Axiom<URI>> getTypes(NamedResource individual, Collection<URI> contexts, boolean includeInferred)
            throws OntoDriverException;

    /**
     * Adds the specified types to the named individual in the ontology.
     *
     * @param individual The types subject
     * @param context    Context into which the type statements will be added
     * @param types      The types to add
     * @throws OntoDriverException When an ontology access error occurs
     */
    void addTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException;

    /**
     * Removes the specified types of the named individual in the ontology.
     *
     * @param individual The types subject
     * @param context    Context into which the type statements will be added
     * @param types      The types to add
     * @throws OntoDriverException When an ontology access error occurs
     */
    void removeTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException;
}
