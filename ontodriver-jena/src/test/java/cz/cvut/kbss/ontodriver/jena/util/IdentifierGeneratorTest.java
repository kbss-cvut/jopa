package cz.cvut.kbss.ontodriver.jena.util;

import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.rdf.model.ResourceFactory;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class IdentifierGeneratorTest {

    private static final URI TYPE_URI = Generator.generateUri();

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private StorageConnector connectorMock;

    private IdentifierGenerator generator;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
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
        when(connectorMock.contains(any(), any(), any(), anyString())).thenReturn(false);
        final URI result = generator.generateIdentifier(TYPE_URI);
        assertNotNull(result);
        verify(connectorMock)
                .contains(ResourceFactory.createResource(result.toString()), ResourceFactory.createProperty(
                        Vocabulary.RDF_TYPE), ResourceFactory.createResource(TYPE_URI.toString()), null);
    }

    @Test
    public void throwsIdentifierGenerationExceptionWhenMaximumAttemptsToGenerateAreExceeded() {
        when(connectorMock.contains(any(), any(), any(), any())).thenReturn(true);
        thrown.expect(IdentifierGenerationException.class);
        generator.generateIdentifier(TYPE_URI);
    }
}