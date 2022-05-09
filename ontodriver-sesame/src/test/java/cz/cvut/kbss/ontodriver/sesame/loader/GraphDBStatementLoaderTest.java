package cz.cvut.kbss.ontodriver.sesame.loader;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.environment.Generator;
import cz.cvut.kbss.ontodriver.sesame.util.AxiomBuilder;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class GraphDBStatementLoaderTest {

    private static final String SUBJECT = Generator.generateUri().toString();

    private static final ValueFactory VF = SimpleValueFactory.getInstance();

    @Mock
    private Connector connector;

    @Mock
    private AxiomBuilder axiomBuilder;

    @Captor
    private ArgumentCaptor<Set<IRI>> captor;

    private StatementLoader sut;

    @BeforeEach
    void setUp() {
        when(connector.getValueFactory()).thenReturn(VF);
        this.sut = new GraphDBStatementLoader(connector, VF.createIRI(SUBJECT), axiomBuilder);
    }

    @Test
    void loadAxiomsAddsGraphDBImplicitAndExplicitContextToLoadingStatementsByAssertionWhenIncludeInferredIsTrue() throws Exception {
        final URI context = Generator.generateUri();
        final Assertion a = Assertion.createObjectPropertyAssertion(Generator.generateUri(), true);
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(SUBJECT));
        descriptor.addAssertion(a);
        descriptor.addAssertionContext(a, context);
        final Map<IRI, Assertion> map = Collections.singletonMap(VF.createIRI(a.getIdentifier().toString()), a);

        sut.setIncludeInferred(true);
        sut.loadAxioms(descriptor, map);
        verify(connector).findStatements(eq(VF.createIRI(SUBJECT)), eq(map.keySet().iterator().next()), isNull(),
                eq(true), captor.capture());
        assertThat(captor.getValue(), hasItem(VF.createIRI(context.toString())));
        assertThat(captor.getValue(),
                hasItem(VF.createIRI(GraphDBStatementLoader.GRAPHDB_IMPLICIT_CONTEXT.toString())));
        assertThat(captor.getValue(),
                hasItem(VF.createIRI(GraphDBStatementLoader.GRAPHDB_EXPLICIT_CONTEXT.toString())));
    }

    @Test
    void loadAxiomsDoesNotAddGraphDBImplicitContextWhenIncludeInferredIsFalse() throws Exception {
        final URI context = Generator.generateUri();
        final Assertion a = Assertion.createObjectPropertyAssertion(Generator.generateUri(), true);
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(SUBJECT));
        descriptor.addAssertion(a);
        descriptor.addAssertionContext(a, context);
        final Map<IRI, Assertion> map = Collections.singletonMap(VF.createIRI(a.getIdentifier().toString()), a);

        sut.setIncludeInferred(false);
        sut.loadAxioms(descriptor, map);
        verify(connector).findStatements(eq(VF.createIRI(SUBJECT)), eq(map.keySet().iterator().next()), isNull(),
                eq(false), captor.capture());
        assertThat(captor.getValue(), hasItem(VF.createIRI(context.toString())));
        assertThat(captor.getValue(),
                not(hasItem(VF.createIRI(GraphDBStatementLoader.GRAPHDB_IMPLICIT_CONTEXT.toString()))));
    }

    @Test
    void loadAxiomsByContextsAddsGraphDBImplicitAndExplicitContextToStatementLoadingWhenIncludeInferredIsTrue() throws Exception {
        final URI context = Generator.generateUri();
        final Assertion a = Assertion.createObjectPropertyAssertion(Generator.generateUri(), true);
        final IRI subjectIri = VF.createIRI(SUBJECT);
        final Statement rdfStatement =
                VF.createStatement(subjectIri, VF.createIRI(a.getIdentifier().toString()),
                        VF.createLiteral(Generator.randomInt()), null);
        when(connector.findStatements(eq(subjectIri), isNull(), isNull(), eq(true), anyCollection())).thenReturn(
                Collections.singleton(rdfStatement));

        sut.setIncludeInferred(true);
        sut.loadAxioms(Collections.singleton(context));
        verify(connector).findStatements(eq(VF.createIRI(SUBJECT)), isNull(), isNull(), eq(true), captor.capture());
        assertThat(captor.getValue(), hasItem(VF.createIRI(context.toString())));
        assertThat(captor.getValue(),
                hasItem(VF.createIRI(GraphDBStatementLoader.GRAPHDB_IMPLICIT_CONTEXT.toString())));
        assertThat(captor.getValue(),
                hasItem(VF.createIRI(GraphDBStatementLoader.GRAPHDB_EXPLICIT_CONTEXT.toString())));
    }

    @Test
    void loadAxiomsByContextsDoesNotAddGraphDBPseudoContextsToStatementLoadingWhenIncludeInferredIsFalse() throws Exception {
        final URI context = Generator.generateUri();
        final Assertion a = Assertion.createObjectPropertyAssertion(Generator.generateUri(), true);
        final IRI subjectIri = VF.createIRI(SUBJECT);
        final Statement rdfStatement =
                VF.createStatement(subjectIri, VF.createIRI(a.getIdentifier().toString()),
                        VF.createLiteral(Generator.randomInt()), null);
        when(connector.findStatements(eq(subjectIri), isNull(), isNull(), eq(false), anyCollection())).thenReturn(
                Collections.singleton(rdfStatement));

        sut.setIncludeInferred(false);
        sut.loadAxioms(Collections.singleton(context));
        verify(connector).findStatements(eq(VF.createIRI(SUBJECT)), isNull(), isNull(), eq(false), captor.capture());
        assertThat(captor.getValue(), hasItem(VF.createIRI(context.toString())));
        assertThat(captor.getValue(),
                not(hasItem(VF.createIRI(GraphDBStatementLoader.GRAPHDB_IMPLICIT_CONTEXT.toString()))));
        assertThat(captor.getValue(),
                not(hasItem(VF.createIRI(GraphDBStatementLoader.GRAPHDB_EXPLICIT_CONTEXT.toString()))));
    }

    @Test
    void contextMatchesReturnsTrueForInferredAssertionAndStatementInDefaultContext() {
        final IRI subjectIri = VF.createIRI(SUBJECT);
        final Assertion a = Assertion.createObjectPropertyAssertion(Generator.generateUri(), true);
        final Statement rdfStatement =
                VF.createStatement(subjectIri, VF.createIRI(a.getIdentifier().toString()),
                        VF.createLiteral(Generator.randomInt()), null);
        sut.setIncludeInferred(true);
        assertTrue(sut.contextMatches(Collections.singleton(Generator.generateUri()), rdfStatement, a));
    }

    @Test
    void contextMatchesReturnsFalseForNonInferredAssertionAndStatementInDefault() {
        final IRI subjectIri = VF.createIRI(SUBJECT);
        final Assertion a = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        final Statement rdfStatement =
                VF.createStatement(subjectIri, VF.createIRI(a.getIdentifier().toString()),
                        VF.createLiteral(Generator.randomInt()), null);
        sut.setIncludeInferred(false);
        assertFalse(sut.contextMatches(Collections.singleton(Generator.generateUri()), rdfStatement, a));
    }
}