/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.owl2java.prefix;

import org.semanticweb.owlapi.model.IRI;

import java.util.Optional;

/**
 * Attempts to resolve ontology prefix by invoking a remote service.
 */
public interface RemotePrefixResolver {

    /**
     * Resolves prefix of an ontology with the specified IRI.
     *
     * @param ontologyIri IRI of the ontology to get prefix for
     * @return Resolved prefix wrapped in {@code Optional}, empty {@code Optional} if unable to get the prefix
     */
    Optional<String> resolvePrefix(IRI ontologyIri);
}
