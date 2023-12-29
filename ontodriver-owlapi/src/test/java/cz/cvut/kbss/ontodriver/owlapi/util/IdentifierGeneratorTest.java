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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class IdentifierGeneratorTest {

    @Mock
    private OWLOntology ontologyMock;

    private IdentifierGenerator generator;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        this.generator = new IdentifierGenerator(ontologyMock);
    }

    @Test
    void generatesUniqueIdentifier() {
        final URI baseUri = URI.create("http://baseUri/");
        when(ontologyMock.containsIndividualInSignature(any(IRI.class))).thenReturn(false);
        final Set<URI> results = new HashSet<>();
        int tries = 100;
        for (int i = 0; i < tries; i++) {
            results.add(generator.generateIdentifier(baseUri));
        }
        results.forEach(uri -> assertTrue(uri.toString().contains(baseUri.toString())));
        assertEquals(tries, results.size());    // All tries lead to unique URI
    }

    @Test
    void throwsExceptionWhenUnableToGenerateUriWithinThreshold() throws Exception {
        final URI baseUri = URI.create("http://baseUri/");
        final int threshold = extractThreshold();
        when(ontologyMock.containsIndividualInSignature(any(IRI.class))).thenReturn(true);
        try {
            assertThrows(IdentifierGenerationException.class, () -> generator.generateIdentifier(baseUri));
        } finally {
            verify(ontologyMock, times(threshold)).containsIndividualInSignature(any(IRI.class));
        }
    }

    private int extractThreshold() throws Exception {
        final Field thresholdField = IdentifierGenerator.class.getDeclaredField("GENERATION_THRESHOLD");
        thresholdField.setAccessible(true);
        return (int) thresholdField.get(null);
    }
}