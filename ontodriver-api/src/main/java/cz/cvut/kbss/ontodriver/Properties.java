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
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

/**
 * This interface provides access to properties not mapped by the object model.
 */
public interface Properties {

    /**
     * Loads property values for the specified individual.
     * <p>
     * This method essentially does the same as
     * {@link Connection#find(cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor)}.
     * It is up to the OOM provider to decide which values are part of the object model and which are unmapped.
     *
     * @param individual      Individual for which property values should be loaded
     * @param context         Context from which to load the property values
     * @param includeInferred Whether to included inferred knowledge
     * @return Collection of axioms representing property values
     * @throws OntoDriverException When storage access error occurs
     */
    Collection<Axiom<?>> getProperties(NamedResource individual, URI context, boolean includeInferred)
            throws OntoDriverException;

    /**
     * Adds the specified property values into the ontology.
     * <p>
     * The property values are either URIs (in case of object properties) or data literals of the appropriate Java type.
     *
     * @param individual Property subject
     * @param context    Context into which to store the property values
     * @param properties The values to add
     * @throws OntoDriverException When storage access error occurs
     */
    void addProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws OntoDriverException;

    /**
     * Removes the specified property values from the ontology.
     * <p>
     * The property values are either URIs (in case of object properties) or data literals of the appropriate Java type.
     *
     * @param individual Property subject
     * @param context    Context from which to remove the property values
     * @param properties The values to remove
     * @throws OntoDriverException When storage access error occurs
     */
    void removeProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws OntoDriverException;

}
