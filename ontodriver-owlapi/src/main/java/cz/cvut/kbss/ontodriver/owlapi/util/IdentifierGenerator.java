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
package cz.cvut.kbss.ontodriver.owlapi.util;

import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.util.IdentifierUtils;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URI;

public class IdentifierGenerator {

    private static final int GENERATION_THRESHOLD = 100;

    private final OWLOntology ontology;

    public IdentifierGenerator(OWLOntology ontology) {
        assert ontology != null;
        this.ontology = ontology;
    }

    /**
     * Generates an identifier which is unique w.r.t. individuals in the known ontology.
     *
     * @param classUri URI of individual's class, used as base for the identifier
     * @return Unique identifier
     * @throws IdentifierGenerationException If unable to generate unique identifier
     */
    public URI generateIdentifier(URI classUri) {
        boolean unique = false;
        URI id = null;
        int counter = 0;
        while (!unique && counter++ < GENERATION_THRESHOLD) {
            id = IdentifierUtils.generateIdentifier(classUri);
            unique = isIdentifierUnique(id);
        }
        if (!unique) {
            throw new IdentifierGenerationException("Unable to generate a unique identifier.");
        }
        return id;
    }

    private boolean isIdentifierUnique(URI uri) {
        return !ontology.containsIndividualInSignature(IRI.create(uri));
    }
}
