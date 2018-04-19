package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.jena.connector.InferredStorageConnector;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.vocabulary.RDF;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static cz.cvut.kbss.ontodriver.util.Vocabulary.RDF_TYPE;
import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public class TypesHandlerTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());
    private static final Resource SUBJECT_RESOURCE = createResource(SUBJECT.getIdentifier().toString());

    private Set<String> types;

    @Mock
    private StorageConnector connectorMock;

    @Mock
    private InferredStorageConnector inferredConnectorMock;

    private TypesHandler handler;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.handler = new TypesHandler(connectorMock, inferredConnectorMock);
        this.types = generateTypes();
    }

    private static Set<String> generateTypes() {
        return IntStream.range(0, 5).mapToObj(i -> Generator.generateUri().toString()).collect(Collectors.toSet());
    }

    @Test
    public void getTypesLoadsTypesFromStorage() {
        final Set<Statement> statements = generateTypesStatements();
        final Set<Axiom<URI>> result = handler.getTypes(SUBJECT, null, false);
        verifyLoadedTypes(statements, result, false);
        verify(inferredConnectorMock, never()).findWithInference(any(), any(), any(), anyString());
        verify(connectorMock).find(SUBJECT_RESOURCE, RDF.type, null, null);
    }

    private Set<Statement> generateTypesStatements() {
        final Set<Statement> statements = statementsForTypes();
        when(connectorMock.find(any(), any(), any(), anyString())).thenReturn(statements);
        return statements;
    }

    private Set<Statement> statementsForTypes() {
        return types.stream().map(t -> ResourceFactory
                .createStatement(SUBJECT_RESOURCE, RDF.type, createResource(t)))
                    .collect(Collectors.toSet());
    }

    private void verifyLoadedTypes(Set<Statement> statements, Set<Axiom<URI>> result, boolean inferred) {
        assertEquals(result.size(), statements.size());
        result.forEach(a -> {
            assertEquals(SUBJECT, a.getSubject());
            assertEquals(Assertion.createClassAssertion(inferred), a.getAssertion());
            assertTrue(types.contains(a.getValue().stringValue()));
        });
    }

    @Test
    public void getTypesLoadsTypesFromContext() {
        final URI context = Generator.generateUri();
        final Set<Statement> statements = generateTypesStatements();
        final Set<Axiom<URI>> result = handler.getTypes(SUBJECT, context, false);
        verifyLoadedTypes(statements, result, false);
        verify(connectorMock).find(SUBJECT_RESOURCE, RDF.type, null, context.toString());
    }

    @Test
    public void addTypesInsertsStatementsIntoStorage() {
        final Set<String> types = generateTypes();
        handler.addTypes(SUBJECT, null, types.stream().map(URI::create).collect(Collectors.toSet()));
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        verifyTypesStatements(types, captor.getValue());
    }

    private void verifyTypesStatements(Set<String> types, List expected) {
        expected.forEach(item -> {
            final Statement statement = (Statement) item;
            assertEquals(SUBJECT_RESOURCE, statement.getSubject());
            assertEquals(createProperty(RDF_TYPE), statement.getPredicate());
            assertTrue(types.contains(statement.getObject().asResource().getURI()));
        });
    }

    @Test
    public void addTypesInsertsStatementsIntoStorageContext() {
        final Set<String> types = generateTypes();
        final URI context = Generator.generateUri();
        handler.addTypes(SUBJECT, context, types.stream().map(URI::create).collect(Collectors.toSet()));
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(context.toString()));
        verifyTypesStatements(types, captor.getValue());
    }

    @Test
    public void removeTypesRemovesStatementsFromStorage() {
        final Set<String> types = generateTypes();
        handler.removeTypes(SUBJECT, null, types.stream().map(URI::create).collect(Collectors.toSet()));
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).remove(captor.capture(), eq(null));
        verifyTypesStatements(types, captor.getValue());
    }

    @Test
    public void removeTypesRemovesStatementsFromStorageContext() {
        final Set<String> types = generateTypes();
        final URI context = Generator.generateUri();
        handler.removeTypes(SUBJECT, context, types.stream().map(URI::create).collect(Collectors.toSet()));
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).remove(captor.capture(), eq(context.toString()));
        verifyTypesStatements(types, captor.getValue());
    }

    @Test
    public void getTypesLoadsTypesWithInferenceWhenInferredSwitchIsOn() {
        final Set<Statement> statements = statementsForTypes();
        when(inferredConnectorMock.findWithInference(any(), any(), any(), anyString())).thenReturn(statements);
        final Set<Axiom<URI>> result = handler.getTypes(SUBJECT, null, true);
        verifyLoadedTypes(statements, result, true);
        verify(connectorMock, never()).find(SUBJECT_RESOURCE, RDF.type, null, null);
        verify(inferredConnectorMock).findWithInference(SUBJECT_RESOURCE, RDF.type, null, null);
    }
}