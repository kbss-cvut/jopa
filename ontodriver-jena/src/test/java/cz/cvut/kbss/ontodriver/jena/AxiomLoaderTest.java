package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class AxiomLoaderTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());

    @Mock
    private StorageConnector connectorMock;

    private AxiomLoader axiomLoader;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.axiomLoader = new AxiomLoader(connectorMock);
    }

    @Test
    public void containsChecksForStatementExistenceInStorage() {
        final String typeUri = Generator.generateUri().toString();
        final Axiom<?> ax = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(typeUri)));
        when(connectorMock.contains(any(), any(), any())).thenReturn(true);
        assertTrue(axiomLoader.contains(ax, null));
        verify(connectorMock)
                .contains(createResource(SUBJECT.getIdentifier().toString()), createProperty(Vocabulary.RDF_TYPE),
                        createResource(typeUri));
    }

    @Test
    public void containsChecksForStatementExistenceInStorageContext() {
        final String typeUri = Generator.generateUri().toString();
        final URI contextUri = Generator.generateUri();
        final Axiom<?> ax = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(typeUri)));
        when(connectorMock.contains(any(), any(), any(), anyString())).thenReturn(false);
        assertFalse(axiomLoader.contains(ax, contextUri));
        verify(connectorMock)
                .contains(createResource(SUBJECT.getIdentifier().toString()), createProperty(Vocabulary.RDF_TYPE),
                        createResource(typeUri), contextUri.toString());
    }
}