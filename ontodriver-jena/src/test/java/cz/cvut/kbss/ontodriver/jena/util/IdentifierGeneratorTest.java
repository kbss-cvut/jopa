/**
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.jena.util;

import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.rdf.model.ResourceFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class IdentifierGeneratorTest {

    private static final URI TYPE_URI = Generator.generateUri();

    @Mock
    private StorageConnector connectorMock;

    private IdentifierGenerator generator;

    @BeforeEach
    public void setUp() {
        MockitoAnnotations.openMocks(this);
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
