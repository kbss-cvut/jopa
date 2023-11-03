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
package cz.cvut.kbss.ontodriver.jena.util;

import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.util.IdentifierUtils;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.ResourceFactory;

import java.net.URI;
import java.util.Collections;

public class IdentifierGenerator {

    private static final int GENERATOR_THRESHOLD = 64;

    private final StorageConnector storageConnector;

    public IdentifierGenerator(StorageConnector storageConnector) {
        this.storageConnector = storageConnector;
    }

    /**
     * Generates a unique identifier based on the specified class URI.
     *
     * @param classUri Type URI, used as the identifier base
     * @return Generated identifier
     */
    public URI generateIdentifier(URI classUri) {
        int i = 0;
        boolean exists;
        final Property property = ResourceFactory.createProperty(Vocabulary.RDF_TYPE);
        final RDFNode type = ResourceFactory.createResource(classUri.toString());
        URI result;
        do {
            result = IdentifierUtils.generateIdentifier(classUri);
            exists = storageConnector.contains(ResourceFactory.createResource(result.toString()), property, type,
                    Collections.emptySet());
            i++;
        } while (exists && i < GENERATOR_THRESHOLD);
        if (i >= GENERATOR_THRESHOLD) {
            throw new IdentifierGenerationException(
                    "Failed to generate a unique identifier in " + GENERATOR_THRESHOLD + " attempts.");
        }
        return result;
    }
}
