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
package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ContainerValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.util.Collection;

/**
 * Interface for managing RDF containers.
 */
public interface Containers {

    /**
     * Reads values from an RDF container specified by the given descriptor.
     *
     * @param descriptor Container descriptor
     * @return Collection of axioms representing the values in the container. The property of the axioms corresponds to
     * the property specified in the container descriptor (i.e., property reference the container from the owner).
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    Collection<Axiom<?>> readContainer(ContainerDescriptor descriptor) throws OntoDriverException;


    /**
     * Persists values to an RDF container specified by the given descriptor.
     *
     * @param descriptor Container descriptor
     * @param <T>        Container value type
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    <T> void persistContainer(ContainerValueDescriptor<T> descriptor) throws OntoDriverException;

    /**
     * Updates values in an RDF container specified by the given descriptor.
     *
     * @param descriptor Container descriptor
     * @param <T>        Container value type
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    <T> void updateContainer(ContainerValueDescriptor<T> descriptor) throws OntoDriverException;
}
