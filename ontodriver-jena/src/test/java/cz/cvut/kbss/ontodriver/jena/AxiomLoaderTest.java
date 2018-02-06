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
import static org.mockito.Matchers.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class AxiomLoaderTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());
    private static final Resource SUBJECT_RES = createResource(SUBJECT.getIdentifier().toString());
    private static final URI CONTEXT = Generator.generateUri();

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
        final List<Statement> statements = generateObjectPropertyAssertions(descriptor.getAssertions());
        when(connectorMock.find(any(), any(), any())).thenReturn(statements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        verifyObjectPropertyAxioms(statements, result);
        verify(connectorMock).find(SUBJECT_RES, null, null);
    }

    private List<Statement> generateObjectPropertyAssertions(Collection<Assertion> assertions) {
        final List<Statement> result = new ArrayList<>();
        assertions.forEach(a -> {
            final Property p = createProperty(a.getIdentifier().toString());
            IntStream.range(0, 2).mapToObj(i -> createResource(Generator.generateUri().toString()))
                     .forEach(r -> result.add(createStatement(SUBJECT_RES, p, r)));
        });
        return result;
    }

    private void verifyObjectPropertyAxioms(List<Statement> expected, Collection<Axiom<?>> actual) {
        assertEquals(expected.size(), actual.size());
        int i = 0;
        for (Axiom<?> axiom : actual) {
            assertEquals(SUBJECT, axiom.getSubject());
            assertEquals(expected.get(i).getPredicate().getURI(), axiom.getAssertion().getIdentifier().toString());
            assertEquals(Assertion.AssertionType.OBJECT_PROPERTY, axiom.getAssertion().getType());
            assertFalse(axiom.getAssertion().isInferred());
            assertEquals(expected.get(i).getObject().asResource().getURI(), axiom.getValue().stringValue());
            i++;
        }
    }

    @Test
    public void findLoadsDataPropertyAxioms() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        descriptor.addAssertion(Assertion.createDataPropertyAssertion(Generator.generateUri(), false));
        descriptor.addAssertion(Assertion.createDataPropertyAssertion(Generator.generateUri(), false));
        final List<Statement> statements = generateDataPropertyAssertions(descriptor.getAssertions());
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

    private List<Statement> generateDataPropertyAssertions(Collection<Assertion> assertions) {
        final List<Statement> result = new ArrayList<>();
        assertions.forEach(a -> {
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
    public void findLoadsStringWhenAssertionHasLanguageTagButValuesDoesNot() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final URI propUri = Generator.generateUri();
        final String lang = "en";
        descriptor.addAssertion(Assertion.createDataPropertyAssertion(propUri, lang, false));
        final Statement statement =
                createStatement(SUBJECT_RES, createProperty(propUri.toString()), createTypedLiteral("a"));
        when(connectorMock.find(any(), any(), any())).thenReturn(Collections.singletonList(statement));

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertEquals(1, result.size());
        assertEquals("a", result.iterator().next().getValue().getValue());
    }

    @Test
    public void findLoadsStringWhenAssertionDoesNotHaveLanguageTagAndValueDoes() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final URI propUri = Generator.generateUri();
        descriptor.addAssertion(Assertion.createDataPropertyAssertion(propUri, false));
        final Statement statement =
                createStatement(SUBJECT_RES, createProperty(propUri.toString()), createLangLiteral("a", "en"));
        when(connectorMock.find(any(), any(), any())).thenReturn(Collections.singletonList(statement));

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertEquals(1, result.size());
        assertEquals("a", result.iterator().next().getValue().getValue());
    }

    @Test
    public void findLoadsStringWhenNeitherAssertionNorValueHaveLanguageTag() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final URI propUri = Generator.generateUri();
        descriptor.addAssertion(Assertion.createDataPropertyAssertion(propUri, false));
        final Statement statement =
                createStatement(SUBJECT_RES, createProperty(propUri.toString()), createTypedLiteral("a"));
        when(connectorMock.find(any(), any(), any())).thenReturn(Collections.singletonList(statement));

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertEquals(1, result.size());
        assertEquals("a", result.iterator().next().getValue().getValue());
    }

    @Test
    public void findLoadsAnnotationPropertyValues() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createAnnotationPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertion(assertion);
        final List<Statement> statements = generateAnnotations(descriptor.getAssertions());
        when(connectorMock.find(any(), any(), any())).thenReturn(statements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        verifyAnnotationPropertyAxioms(statements, result);
    }

    private void verifyAnnotationPropertyAxioms(List<Statement> expected, Collection<Axiom<?>> actual) {
        assertEquals(expected.size(), actual.size());
        int i = 0;
        for (Axiom<?> axiom : actual) {
            if (expected.get(i).getObject().isResource()) {
                assertEquals(expected.get(i).getObject().asResource().getURI(), axiom.getValue().stringValue());
            } else {
                assertEquals(expected.get(i).getObject().asLiteral().getValue(), axiom.getValue().getValue());
            }
            i++;
        }
    }

    private List<Statement> generateAnnotations(Collection<Assertion> assertions) {
        final List<Statement> list = new ArrayList<>();
        assertions.forEach(a -> {
            final Property p = createProperty(a.getIdentifier().toString());
            list.add(createStatement(SUBJECT_RES, p, createTypedLiteral(Generator.randomInt())));
            list.add(createStatement(SUBJECT_RES, p, createResource(Generator.generateUri().toString())));
        });
        return list;
    }

    @Test
    public void findLoadsAllStatementsWhenDescriptorContainsUnspecifiedProperty() {
        final Assertion dp = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        final List<Statement> statements = generateDataPropertyAssertions(Collections.singletonList(dp));
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
        final List<Statement> statements = generateDataPropertyAssertions(descriptor.getAssertions());
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
        final List<Statement> dpStatements = generateDataPropertyAssertions(descriptor.getAssertions());
        when(connectorMock.find(any(), any(), any())).thenReturn(dpStatements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertTrue(result.isEmpty());
    }

    @Test
    public void findSkipsResourcesWhenLoadingDataPropertyValues() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final URI property = Generator.generateUri();
        descriptor.addAssertion(Assertion.createDataPropertyAssertion(property, false));
        final List<Statement> opStatements = generateObjectPropertyAssertions(descriptor.getAssertions());
        when(connectorMock.find(any(), any(), any())).thenReturn(opStatements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertTrue(result.isEmpty());
    }

    @Test
    public void findSkipsClassAssertionsWhenLoadingUnspecifiedPropertyValues() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        descriptor.addAssertion(Assertion.createUnspecifiedPropertyAssertion(false));
        final List<Statement> statements = generateClassAssertions();
        when(connectorMock.find(any(), any(), any())).thenReturn(statements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertTrue(result.isEmpty());
    }

    @Test
    public void findLoadsClassAssertionStatementsFromContext() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        descriptor.addAssertion(Assertion.createClassAssertion(false));
        descriptor.setSubjectContext(CONTEXT);
        final Collection<Statement> statements = generateClassAssertions();
        when(connectorMock.find(any(), any(), any(), eq(CONTEXT.toString()))).thenReturn(statements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertEquals(statements.size(), result.size());
        verify(connectorMock).find(SUBJECT_RES, null, null, CONTEXT.toString());
    }

    @Test
    public void findLoadsAssertionValuesFromContext() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertion(assertion);
        descriptor.setAssertionContext(assertion, CONTEXT);
        final List<Statement> statements = generateObjectPropertyAssertions(descriptor.getAssertions());
        when(connectorMock.find(any(), any(), any(), eq(CONTEXT.toString()))).thenReturn(statements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        verifyObjectPropertyAxioms(statements, result);
        verify(connectorMock)
                .find(SUBJECT_RES, createProperty(assertion.getIdentifier().toString()), null, CONTEXT.toString());
    }

    @Test
    public void findSkipsStatementsWhichAreInDifferentContextThanAssertionExpects() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertion(assertion);
        descriptor.setAssertionContext(assertion, CONTEXT);

        final List<Statement> matching = generateDataPropertyAssertions(descriptor.getAssertions());
        final List<Statement> notMatching = generateDataPropertyAssertions(descriptor.getAssertions());
        when(connectorMock
                .find(SUBJECT_RES, createProperty(assertion.getIdentifier().toString()), null, CONTEXT.toString()))
                .thenReturn(matching);
        when(connectorMock.find(any(), any(), any())).thenReturn(notMatching);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertEquals(matching.size(), result.size());
        int i = 0;
        for (Axiom<?> axiom : result) {
            assertEquals(matching.get(i).getObject().asLiteral().getValue(), axiom.getValue().getValue());
            i++;
        }
        verify(connectorMock)
                .find(SUBJECT_RES, createProperty(assertion.getIdentifier().toString()), null, CONTEXT.toString());
    }

    @Test
    public void findSkipsPropertyValuesWhenUnspecifiedPropertyHasDifferentContext() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final Assertion ap = Assertion.createAnnotationPropertyAssertion(Generator.generateUri(), false);
        final List<Statement> notMatching = generateAnnotations(Collections.singletonList(ap));
        final URI anotherContext = Generator.generateUri();
        when(connectorMock
                .find(SUBJECT_RES, createProperty(ap.getIdentifier().toString()), null, anotherContext.toString()))
                .thenReturn(notMatching);
        final Assertion unspecified = Assertion.createUnspecifiedPropertyAssertion(false);
        final List<Statement> matching = generateAnnotations(Collections.singletonList(unspecified));
        when(connectorMock.find(SUBJECT_RES, null, null, CONTEXT.toString())).thenReturn(matching);
        descriptor.addAssertion(unspecified);
        descriptor.setAssertionContext(unspecified, CONTEXT);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        verifyAnnotationPropertyAxioms(matching, result);
    }

    @Test
    public void findLoadsMultipleKindsOfProperties() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final Assertion clsAssertion = Assertion.createClassAssertion(false);
        descriptor.addAssertion(clsAssertion);
        final List<Statement> clsStatements = generateClassAssertions();
        final Assertion dpAssertion = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertion(dpAssertion);
        final List<Statement> dpStatements = generateDataPropertyAssertions(Collections.singletonList(dpAssertion));
        final Assertion opAssertion = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertion(opAssertion);
        final List<Statement> opStatements = generateObjectPropertyAssertions(Collections.singletonList(opAssertion));
        final Assertion apAssertion = Assertion.createAnnotationPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertion(apAssertion);
        final List<Statement> apStatements = generateAnnotations(Collections.singletonList(apAssertion));
        final List<Statement> allStatements = new ArrayList<>(clsStatements);
        allStatements.addAll(dpStatements);
        allStatements.addAll(opStatements);
        allStatements.addAll(apStatements);
        when(connectorMock.find(SUBJECT_RES, null, null)).thenReturn(allStatements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertEquals(allStatements.size(), result.size());
    }

    @Test
    public void findLoadsDataPropertyValueWhenItIsNotStringAndAssertionHasLanguage() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createDataPropertyAssertion(Generator.generateUri(), "en", false);
        descriptor.addAssertion(assertion);
        final List<Statement> statements = generateDataPropertyAssertions(Collections.singleton(assertion));
        when(connectorMock.find(SUBJECT_RES, null, null)).thenReturn(statements);

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertEquals(statements.size(), result.size());
    }

    @Test
    public void findLoadsDataPropertyWithLongValueAsLong() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createDataPropertyAssertion(Generator.generateUri(), "en", false);
        descriptor.addAssertion(assertion);
        final Long value = 117L;
        final Statement statement = createStatement(SUBJECT_RES, createProperty(assertion.getIdentifier().toString()),
                createTypedLiteral(value));
        when(connectorMock.find(any(), any(), any())).thenReturn(Collections.singleton(statement));

        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertEquals(1, result.size());
        final Axiom<?> axiom = result.iterator().next();
        assertEquals(value, axiom.getValue().getValue());
    }
}