/**
 * Copyright (C) 2019 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

class IdentifierGeneratorTest {

    @Mock
    private OWLOntology ontologyMock;

    private IdentifierGenerator generator;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.initMocks(this);
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