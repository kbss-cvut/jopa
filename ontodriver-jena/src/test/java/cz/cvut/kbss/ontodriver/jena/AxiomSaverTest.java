package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;

public class AxiomSaverTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());

    @Mock
    private StorageConnector connectorMock;

    private AxiomSaver saver;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.saver = new AxiomSaver(connectorMock);
    }

    @Test
    public void saveAxiomsAddsClassAssertionToStorage() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion a = Assertion.createClassAssertion(false);
        final NamedResource type = NamedResource.create(Generator.generateUri());
        descriptor.addAssertionValue(a, new Value<>(type));
        saver.saveAxioms(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture());
        final List<Statement> arg = captor.getValue();
        assertEquals(1, arg.size());
        assertEquals(ResourceFactory.createResource(SUBJECT.toString()), arg.get(0).getSubject());
        assertEquals(ResourceFactory.createProperty(Vocabulary.RDF_TYPE), arg.get(0).getPredicate());
        assertEquals(ResourceFactory.createResource(type.getIdentifier().toString()), arg.get(0).getObject());
    }

    @Test
    public void saveAxiomsAddsResourceStatementsForObjectPropertyAssertionAxioms() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        final NamedResource value = NamedResource.create(Generator.generateUri());
        descriptor.addAssertionValue(assertion, new Value<>(value));
        saver.saveAxioms(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture());
        final List<Statement> arg = captor.getValue();
        assertEquals(1, arg.size());
        assertEquals(ResourceFactory.createProperty(assertion.getIdentifier().toString()), arg.get(0).getPredicate());
        assertEquals(ResourceFactory.createResource(value.getIdentifier().toString()), arg.get(0).getObject());
    }

    @Test
    public void saveAxiomsAddsLiteralStatementsForDatatypePropertyAssertionAxioms() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        final Integer value = 117;
        descriptor.addAssertionValue(assertion, new Value<>(value));
        saver.saveAxioms(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture());
        final List<Statement> arg = captor.getValue();
        assertEquals(1, arg.size());
        assertEquals(ResourceFactory.createTypedLiteral(value), arg.get(0).getObject());
    }

    @Test
    public void saveAxiomsCreatesLanguageTaggedLiteralsForDatatypePropertyAssertionsWithLanguageTag() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final String lang = "en";
        final Assertion assertion = Assertion.createDataPropertyAssertion(Generator.generateUri(), lang, false);
        final String value = "english text";
        descriptor.addAssertionValue(assertion, new Value<>(value));
        saver.saveAxioms(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture());
        final List<Statement> arg = captor.getValue();
        assertEquals(1, arg.size());
        assertEquals(ResourceFactory.createLangLiteral(value, lang), arg.get(0).getObject());
    }

    @Test
    public void saveAxiomsAddsResourceStatementsForAnnotationPropertyAxiomsWithIriValue() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createAnnotationPropertyAssertion(Generator.generateUri(), false);
        final String value = Generator.generateUri().toString();
        descriptor.addAssertionValue(assertion, new Value<>(value));
        saver.saveAxioms(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture());
        final List<Statement> arg = captor.getValue();
        assertEquals(1, arg.size());
        assertEquals(ResourceFactory.createResource(value), arg.get(0).getObject());
    }

    @Test
    public void saveAxiomsAddsLiteralStatementForAnnotationPropertyAxiomWithLiteralValue() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createAnnotationPropertyAssertion(Generator.generateUri(), false);
        final Integer value = 117;
        descriptor.addAssertionValue(assertion, new Value<>(value));
        saver.saveAxioms(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture());
        final List<Statement> arg = captor.getValue();
        assertEquals(1, arg.size());
        assertEquals(ResourceFactory.createTypedLiteral(value), arg.get(0).getObject());
    }

    @Test
    public void saveAxiomsAddsStatementsIntoCorrectContextWhenAssertionHasContext() {
        final URI context = Generator.generateUri();
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        final Integer value = 117;
        descriptor.addAssertionValue(assertion, new Value<>(value));
        descriptor.setAssertionContext(assertion, context);
        saver.saveAxioms(descriptor);
        verify(connectorMock).add(anyListOf(Statement.class), eq(context.toString()));
    }

    @Test
    public void saveAxiomsSkipsNullValues() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertionValue(assertion, Value.nullValue());
        saver.saveAxioms(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture());
        assertTrue(captor.getValue().isEmpty());
    }
}