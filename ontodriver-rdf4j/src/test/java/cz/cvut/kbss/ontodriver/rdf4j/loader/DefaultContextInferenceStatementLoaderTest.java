package cz.cvut.kbss.ontodriver.rdf4j.loader;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import cz.cvut.kbss.ontodriver.rdf4j.util.AxiomBuilder;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class DefaultContextInferenceStatementLoaderTest {

    private static final String SUBJECT = Generator.generateUri().toString();

    private static final ValueFactory VF = SimpleValueFactory.getInstance();

    @Mock
    private Connector connector;

    @Mock
    private AxiomBuilder axiomBuilder;

    private DefaultContextInferenceStatementLoader sut;

    @BeforeEach
    void setUp() {
        when(connector.getValueFactory()).thenReturn(VF);
        this.sut = new DefaultContextInferenceStatementLoader(connector, VF.createIRI(SUBJECT), axiomBuilder);
    }

    @Test
    void resolveContextsReturnsEmptySetWhenAssertionIsInferredAndIncludeInferredIsTrue() {
        final Assertion a = Assertion.createObjectPropertyAssertion(Generator.generateUri(), true);
        final URI context = Generator.generateUri();
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(SUBJECT));
        descriptor.addAssertion(a);
        descriptor.addAssertionContext(a, context);
        sut.setIncludeInferred(true);

        final Set<URI> result = sut.resolveContexts(descriptor, a);
        assertTrue(result.isEmpty());
    }

    @Test
    void resolveContextsReturnsAssertionContextsWhenAssertionIsNotInferred() {
        final Assertion a = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        final URI context = Generator.generateUri();
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(SUBJECT));
        descriptor.addAssertion(a);
        descriptor.addAssertionContext(a, context);

        final Set<URI> result = sut.resolveContexts(descriptor, a);
        assertEquals(Collections.singleton(context), result);
    }

    @Test
    void contextMatchesReturnsTrueWhenAssertionIsInferredAndIncludeInferredIsTrue() {
        final Assertion a = Assertion.createObjectPropertyAssertion(Generator.generateUri(), true);
        final Statement rdfStatement =
                VF.createStatement(VF.createIRI(SUBJECT), VF.createIRI(a.getIdentifier().toString()),
                                   VF.createLiteral(Generator.randomInt()), null);
        sut.setIncludeInferred(true);
        assertTrue(sut.contextMatches(Collections.singleton(Generator.generateUri()), rdfStatement, a));
    }

    @Test
    void contextMatchesReturnsFalseWhenAssertionIsNotInferredAndContextDoesNotMatch() {
        final Assertion a = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        final Statement rdfStatement =
                VF.createStatement(VF.createIRI(SUBJECT), VF.createIRI(a.getIdentifier().toString()),
                                   VF.createLiteral(Generator.randomInt()), null);
        assertFalse(sut.contextMatches(Collections.singleton(Generator.generateUri()), rdfStatement, a));
    }

    /**
     * Corresponds to using the default context
     */
    @Test
    void loadAxiomsByContextsPassesNoContextsToConnectorWhenIncludeInferredIsTrue() throws Exception {
        final List<Statement> statements = Arrays.asList(
                VF.createStatement(VF.createIRI(SUBJECT), RDF.TYPE,
                                   VF.createIRI(Generator.generateUri().toString()), null),
                VF.createStatement(VF.createIRI(SUBJECT), RDF.TYPE,
                                   VF.createIRI(Generator.generateUri().toString()),
                                   VF.createIRI(Generator.generateUri().toString()))
        );
        when(connector.findStatements(eq(VF.createIRI(SUBJECT)), any(), any(), eq(true), anyCollection())).thenReturn(
                statements);

        sut.setIncludeInferred(true);
        sut.loadAxioms(Collections.singleton(Generator.generateUri()));
        statements.forEach(s -> verify(axiomBuilder).statementToAxiom(s));
        verify(connector).findStatements(VF.createIRI(SUBJECT), null, null, true, Collections.emptySet());
    }
}
