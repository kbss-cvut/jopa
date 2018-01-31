package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class AxiomLoaderTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());
    private static final Resource SUBJECT_RES = createResource(SUBJECT.getIdentifier().toString());

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
        verify(connectorMock).contains(SUBJECT_RES, createProperty(Vocabulary.RDF_TYPE), createResource(typeUri));
    }

    @Test
    public void containsChecksForStatementExistenceInStorageContext() {
        final String typeUri = Generator.generateUri().toString();
        final URI contextUri = Generator.generateUri();
        final Axiom<?> ax = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(typeUri)));
        when(connectorMock.contains(any(), any(), any(), anyString())).thenReturn(false);
        assertFalse(axiomLoader.contains(ax, contextUri));
        verify(connectorMock).contains(SUBJECT_RES, createProperty(Vocabulary.RDF_TYPE), createResource(typeUri),
                contextUri.toString());
    }

    @Test
    public void findLoadsClassAssertionValues() {
        final List<Statement> statements = generateClassAssertions();
        when(connectorMock.find(any(), any(), any())).thenReturn(statements);
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        descriptor.addAssertion(Assertion.createClassAssertion(false));

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertEquals(statements.size(), result.size());
        int i = 0;
        for (Axiom<?> axiom : result) {
            assertEquals(SUBJECT, axiom.getSubject());
            assertTrue(axiom.getAssertion().isClassAssertion());
            assertFalse(axiom.getAssertion().isInferred());
            assertEquals(statements.get(i).getObject().asResource().getURI(), axiom.getValue().stringValue());
            i++;
        }
        verify(connectorMock).find(SUBJECT_RES, null, null);
    }

    private List<Statement> generateClassAssertions() {
        final Property property = createProperty(Vocabulary.RDF_TYPE);
        return IntStream.range(0, 3).mapToObj(i -> ResourceFactory
                .createStatement(SUBJECT_RES, property, createResource(Generator.generateUri().toString()))).collect(
                Collectors.toList());
    }

    @Test
    public void findLoadsObjectPropertyAxioms() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        descriptor.addAssertion(Assertion.createObjectPropertyAssertion(Generator.generateUri(), false));
        descriptor.addAssertion(Assertion.createObjectPropertyAssertion(Generator.generateUri(), false));
        final List<Statement> statements = generateObjectPropertyAssertions(descriptor);
        when(connectorMock.find(any(), any(), any())).thenReturn(statements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertEquals(statements.size(), result.size());
        int i = 0;
        for (Axiom<?> axiom : result) {
            assertEquals(SUBJECT, axiom.getSubject());
            assertEquals(statements.get(i).getPredicate().getURI(), axiom.getAssertion().getIdentifier().toString());
            assertEquals(Assertion.AssertionType.OBJECT_PROPERTY, axiom.getAssertion().getType());
            assertFalse(axiom.getAssertion().isInferred());
            assertEquals(statements.get(i).getObject().asResource().getURI(), axiom.getValue().stringValue());
            i++;
        }
        verify(connectorMock).find(SUBJECT_RES, null, null);
    }

    private List<Statement> generateObjectPropertyAssertions(AxiomDescriptor descriptor) {
        final List<Statement> result = new ArrayList<>();
        descriptor.getAssertions().forEach(a -> {
            final Property p = createProperty(a.getIdentifier().toString());
            IntStream.range(0, 2).mapToObj(i -> createResource(Generator.generateUri().toString()))
                     .forEach(r -> result.add(createStatement(SUBJECT_RES, p, r)));
        });
        return result;
    }

    @Test
    public void findLoadsDataPropertyAxioms() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        descriptor.addAssertion(Assertion.createDataPropertyAssertion(Generator.generateUri(), false));
        descriptor.addAssertion(Assertion.createDataPropertyAssertion(Generator.generateUri(), false));
        final List<Statement> statements = generateDataPropertyAssertions(descriptor);
        when(connectorMock.find(any(), any(), any())).thenReturn(statements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertEquals(statements.size(), result.size());
        int i = 0;
        for (Axiom<?> axiom : result) {
            assertEquals(statements.get(i).getPredicate().getURI(), axiom.getAssertion().getIdentifier().toString());
            assertEquals(Assertion.AssertionType.DATA_PROPERTY, axiom.getAssertion().getType());
            assertEquals(statements.get(i).getObject().asLiteral().getValue(), axiom.getValue().getValue());
            i++;
        }
    }

    private List<Statement> generateDataPropertyAssertions(AxiomDescriptor descriptor) {
        final List<Statement> result = new ArrayList<>();
        descriptor.getAssertions().forEach(a -> {
            final Property p = createProperty(a.getIdentifier().toString());
            result.add(createStatement(SUBJECT_RES, p, createTypedLiteral(Generator.randomInt())));
            result.add(createStatement(SUBJECT_RES, p, createTypedLiteral(Generator.randomInt() % 2 == 0)));
        });
        return result;
    }

    @Test
    public void findSkipsAnonymousPropertyValues() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final URI propUri = Generator.generateUri();
        descriptor.addAssertion(Assertion.createObjectPropertyAssertion(propUri, false));
        final Statement statement = createStatement(SUBJECT_RES, createProperty(propUri.toString()), createResource());
        when(connectorMock.find(any(), any(), any())).thenReturn(Collections.singletonList(statement));

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertTrue(result.isEmpty());
    }

    @Test
    public void findSkipsStringValuesWithIncorrectLanguageTag() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final URI propUri = Generator.generateUri();
        final String lang = "en";
        descriptor.addAssertion(Assertion.createDataPropertyAssertion(propUri, lang, false));
        final Statement matching =
                createStatement(SUBJECT_RES, createProperty(propUri.toString()), createLangLiteral("a", lang));
        final Statement notMatching =
                createStatement(SUBJECT_RES, createProperty(propUri.toString()), createLangLiteral("b", "cs"));
        when(connectorMock.find(any(), any(), any())).thenReturn(Arrays.asList(matching, notMatching));

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertEquals(1, result.size());
        final Axiom<?> axiom = result.iterator().next();
        assertEquals(matching.getObject().asLiteral().getValue(), axiom.getValue().getValue());
    }

    @Test
    public void findLoadsAnnotationPropertyValues() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createAnnotationPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertion(assertion);
        final List<Statement> statements = generateAnnotations(descriptor);
        when(connectorMock.find(any(), any(), any())).thenReturn(statements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertEquals(statements.size(), result.size());
        int i = 0;
        for (Axiom<?> axiom : result) {
            assertEquals(assertion, axiom.getAssertion());
            if (statements.get(i).getObject().isResource()) {
                assertEquals(statements.get(i).getObject().asResource().getURI(), axiom.getValue().stringValue());
            } else {
                assertEquals(statements.get(i).getObject().asLiteral().getValue(), axiom.getValue().getValue());
            }
            i++;
        }
    }

    private List<Statement> generateAnnotations(AxiomDescriptor descriptor) {
        final List<Statement> list = new ArrayList<>();
        descriptor.getAssertions().forEach(a -> {
            final Property p = createProperty(a.getIdentifier().toString());
            list.add(createStatement(SUBJECT_RES, p, createTypedLiteral(Generator.randomInt())));
            list.add(createStatement(SUBJECT_RES, p, createResource(Generator.generateUri().toString())));
        });
        return list;
    }

    @Test
    public void findLoadsAllStatementsWhenDescriptorContainsUnspecifiedProperty() {
        final AxiomDescriptor temp = new AxiomDescriptor(SUBJECT);
        final Assertion dp = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        temp.addAssertion(dp);
        final List<Statement> statements = generateDataPropertyAssertions(temp);
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        descriptor.addAssertion(Assertion.createUnspecifiedPropertyAssertion(false));
        when(connectorMock.find(any(), any(), any())).thenReturn(statements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertEquals(statements.size(), result.size());
        int i = 0;
        for (Axiom<?> axiom : result) {
            assertEquals(dp, axiom.getAssertion());
            assertEquals(statements.get(i).getObject().asLiteral().getValue(), axiom.getValue().getValue());
            i++;
        }
    }

    @Test
    public void findSkipsStatementsForWhichAssertionIsNotPresentInDescriptor() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        descriptor.addAssertion(Assertion.createDataPropertyAssertion(Generator.generateUri(), false));
        final List<Statement> statements = generateDataPropertyAssertions(descriptor);
        final Assertion unwanted = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        statements.add(createStatement(SUBJECT_RES, createProperty(unwanted.getIdentifier().toString()),
                createResource(Generator.generateUri().toString())));
        when(connectorMock.find(any(), any(), any())).thenReturn(statements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        final Optional<Axiom<?>> found = result.stream().filter(ax -> unwanted.equals(ax.getAssertion())).findAny();
        assertFalse(found.isPresent());
    }

    @Test
    public void findSkipsLiteralsWhenLoadingObjectPropertyValues() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final URI property = Generator.generateUri();
        descriptor.addAssertion(Assertion.createObjectPropertyAssertion(property, false));
        final List<Statement> dpStatements = generateDataPropertyAssertions(descriptor);
        when(connectorMock.find(any(), any(), any())).thenReturn(dpStatements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertTrue(result.isEmpty());
    }

    @Test
    public void findSkipsResourcesWhenLoadingDataPropertyValues() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final URI property = Generator.generateUri();
        descriptor.addAssertion(Assertion.createDataPropertyAssertion(property, false));
        final List<Statement> opStatements = generateObjectPropertyAssertions(descriptor);
        when(connectorMock.find(any(), any(), any())).thenReturn(opStatements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertTrue(result.isEmpty());
    }
}