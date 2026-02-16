/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.rdf.model.ResourceFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class IdentifierGeneratorTest {

    private static final URI TYPE_URI = Generator.generateUri();

    @Mock
    private StorageConnector connectorMock;

    private IdentifierGenerator generator;

    @BeforeEach
    public void setUp() {
        this.generator = new IdentifierGenerator(connectorMock);
    }

    @Test
    public void generateIdentifierCreatesIdentifierBasedOnClassUri() {
        final URI result = generator.generateIdentifier(TYPE_URI);
        assertNotNull(result);
        assertTrue(result.toString().contains(TYPE_URI.toString()));
    }

    @Test
    public void generateIdentifierChecksForExistenceOfNewlyGeneratedIdentifier() {
        when(connectorMock.contains(any(), any(), any(), anySet())).thenReturn(false);
        final URI result = generator.generateIdentifier(TYPE_URI);
        assertNotNull(result);
        verify(connectorMock)
                .contains(ResourceFactory.createResource(result.toString()), ResourceFactory.createProperty(
                        Vocabulary.RDF_TYPE), ResourceFactory.createResource(TYPE_URI.toString()),
                        Collections.emptySet());
    }

    @Test
    public void throwsIdentifierGenerationExceptionWhenMaximumAttemptsToGenerateAreExceeded() {
        when(connectorMock.contains(any(), any(), any(), any())).thenReturn(true);
        assertThrows(IdentifierGenerationException.class, () -> generator.generateIdentifier(TYPE_URI));
    }
}
