package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;

public class PropertiesHandlerTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());
    private static final Resource SUBJECT_RESOURCE = createResource(SUBJECT.getIdentifier().toString());

    @Mock
    private StorageConnector connectorMock;

    private PropertiesHandler handler;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.handler = new PropertiesHandler(connectorMock, null);
    }

    @Test
    public void getPropertiesLoadsPropertyValues() {
        final Property pOne = createProperty(Generator.generateUri().toString());
        final Property pTwo = createProperty(Generator.generateUri().toString());
        final Integer vOne = 117;
        final URI vTwo = Generator.generateUri();
        final List<Statement> statements = Arrays
                .asList(createStatement(SUBJECT_RESOURCE, pOne, ResourceFactory.createTypedLiteral(vOne)),
                        createStatement(SUBJECT_RESOURCE, pTwo, createResource(vTwo.toString())));
        when(connectorMock.find(SUBJECT_RESOURCE, null, null, null)).thenReturn(statements);

        final Collection<Axiom<?>> result = handler.getProperties(SUBJECT, null, false);
        final List<Axiom<?>> lResult = new ArrayList<>(result);
        assertEquals(2, result.size());
        assertEquals(Assertion.createDataPropertyAssertion(URI.create(pOne.getURI()), false),
                lResult.get(0).getAssertion());
        assertEquals(new Value<>(vOne), lResult.get(0).getValue());
        assertEquals(Assertion.createObjectPropertyAssertion(URI.create(pTwo.getURI()), false),
                lResult.get(1).getAssertion());
        assertEquals(new Value<>(NamedResource.create(vTwo)), lResult.get(1).getValue());
    }

    @Test
    public void getPropertiesLoadsPropertyValuesFromContext() {
        final Property pOne = createProperty(Generator.generateUri().toString());
        final Integer vOne = 117;
        final String context = Generator.generateUri().toString();
        final List<Statement> statements = Collections
                .singletonList(createStatement(SUBJECT_RESOURCE, pOne, ResourceFactory.createTypedLiteral(vOne)));
        when(connectorMock.find(SUBJECT_RESOURCE, null, null, context)).thenReturn(statements);

        final Collection<Axiom<?>> result = handler.getProperties(SUBJECT, URI.create(context), false);
        final List<Axiom<?>> lResult = new ArrayList<>(result);
        assertEquals(1, result.size());
        assertEquals(Assertion.createDataPropertyAssertion(URI.create(pOne.getURI()), false),
                lResult.get(0).getAssertion());
        assertEquals(new Value<>(vOne), lResult.get(0).getValue());
        verify(connectorMock, never()).find(any(), any(), any(),eq(null));
    }

    @Test
    public void addPropertiesAddsStatementsIntoRepository() {
        final Assertion a = Assertion.createPropertyAssertion(Generator.generateUri(), false);
        final Integer vOne = 117;
        final Integer vTwo = 118;
        final Set<Integer> values = new HashSet<>(Arrays.asList(vOne, vTwo));

        handler.addProperties(SUBJECT, null,
                Collections.singletonMap(a, new HashSet<>(Arrays.asList(new Value<>(vOne), new Value<>(vTwo)))));
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        assertEquals(2, captor.getValue().size());
        captor.getValue().forEach(item -> {
            final Statement s = (Statement) item;
            assertEquals(SUBJECT_RESOURCE, s.getSubject());
            assertEquals(createProperty(a.getIdentifier().toString()), s.getPredicate());
            assertTrue(s.getObject().isLiteral());
            assertTrue(values.contains(s.getObject().asLiteral().getValue()));
        });
    }

    @Test
    public void addPropertiesAddsStatementsIntoCorrectContext() {
        final Assertion a = Assertion.createPropertyAssertion(Generator.generateUri(), false);
        final URI vOne = Generator.generateUri();
        final URI vTwo = Generator.generateUri();
        final Set<URI> values = new HashSet<>(Arrays.asList(vOne, vTwo));
        final URI context = Generator.generateUri();

        handler.addProperties(SUBJECT, context,
                Collections.singletonMap(a, new HashSet<>(Arrays.asList(new Value<>(NamedResource.create(vOne)),
                        new Value<>(NamedResource.create(vTwo))))));
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(context.toString()));
        assertEquals(2, captor.getValue().size());
        captor.getValue().forEach(item -> {
            final Statement s = (Statement) item;
            assertEquals(SUBJECT_RESOURCE, s.getSubject());
            assertEquals(createProperty(a.getIdentifier().toString()), s.getPredicate());
            assertTrue(s.getObject().isResource());
            assertTrue(values.contains(URI.create(s.getObject().asResource().getURI())));
        });
    }

    @Test
    public void removePropertiesDeletesStatementsFromRepository() {
        final Assertion a = Assertion.createPropertyAssertion(Generator.generateUri(), false);
        final Integer vOne = 117;
        final Integer vTwo = 118;

        handler.removeProperties(SUBJECT, null,
                Collections.singletonMap(a, new HashSet<>(Arrays.asList(new Value<>(vOne), new Value<>(vTwo)))));
        verify(connectorMock)
                .remove(SUBJECT_RESOURCE, createProperty(a.getIdentifier().toString()), createTypedLiteral(vOne), null);
        verify(connectorMock)
                .remove(SUBJECT_RESOURCE, createProperty(a.getIdentifier().toString()), createTypedLiteral(vTwo), null);
    }

    @Test
    public void removePropertiesDeletesStatementsFromCorrectContext() {
        final Assertion a = Assertion.createPropertyAssertion(Generator.generateUri(), false);
        final String vOne = Generator.generateUri().toString();
        final String vTwo = Generator.generateUri().toString();
        final URI context = Generator.generateUri();

        handler.removeProperties(SUBJECT, context,
                Collections.singletonMap(a, new HashSet<>(Arrays.asList(new Value<>(NamedResource.create(vOne)),
                        new Value<>(NamedResource.create(vTwo))))));
        verify(connectorMock)
                .remove(SUBJECT_RESOURCE, createProperty(a.getIdentifier().toString()), createResource(vOne),
                        context.toString());
        verify(connectorMock)
                .remove(SUBJECT_RESOURCE, createProperty(a.getIdentifier().toString()), createResource(vTwo),
                        context.toString());
    }
}