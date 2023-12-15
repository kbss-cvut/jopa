package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.MultilingualString;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ReferencedListIteratorTest {

    private static final ValueFactory VF = SimpleValueFactory.getInstance();
    private static ReferencedListDescriptor descriptor;

    @Mock
    private Connector connector;

    @BeforeAll
    static void setUpBeforeAll() {
        descriptor = new ReferencedListDescriptorImpl(ListHandlerTestHelper.OWNER, ReferencedListHandlerTest.hasListAssertion,
                ReferencedListHandlerTest.nextNodeAssertion, ReferencedListHandlerTest.nodeContentAssertion);
    }

    @Test
    void nextNodeThrowsIntegrityConstraintViolatedExceptionWhenNodeHasMultipleContentValues() throws Exception {
        final IRI node = VF.createIRI(Generator.generateUri().toString());
        final Statement nextStatement = VF.createStatement(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, node);
        when(connector.findStatements(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, null, false, Collections.emptySet())).thenReturn(List.of(nextStatement));
        when(connector.findStatements(node, ReferencedListHandlerTest.nodeContentProperty, null, false, Collections.emptySet())).thenReturn(
                List.of(VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createIRI(Generator.generateUri()
                                                                                                                      .toString())),
                        VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createIRI(Generator.generateUri()
                                                                                                                      .toString())))
        );
        final ReferencedListIterator<NamedResource> sut = new ReferencedListIterator<>(descriptor, connector, VF);

        assertThrows(IntegrityConstraintViolatedException.class, sut::nextNode);
    }

    @Test
    void nextNodeAllowsMultipleTranslationsOfStringAsNodeContentValues() throws Exception {
        final IRI node = VF.createIRI(Generator.generateUri().toString());
        final Statement nextStatement = VF.createStatement(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, node);
        when(connector.findStatements(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, null, false, Collections.emptySet())).thenReturn(List.of(nextStatement));
        when(connector.findStatements(node, ReferencedListHandlerTest.nodeContentProperty, null, false, Collections.emptySet())).thenReturn(
                List.of(VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("One", "en")),
                        VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("Jedna", "cs")))
        );
        final ReferencedListIterator<MultilingualString> sut = new ReferencedListIterator<>(descriptor, connector, VF);

        assertDoesNotThrow(sut::nextNode);
    }

    @Test
    void nextAxiomReturnsAxiomWithMultilingualStringWhenNodeContentIsMultilingualString() throws Exception {
        final IRI node = VF.createIRI(Generator.generateUri().toString());
        final Statement nextStatement = VF.createStatement(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, node);
        when(connector.findStatements(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, null, false, Collections.emptySet())).thenReturn(List.of(nextStatement));
        when(connector.findStatements(node, ReferencedListHandlerTest.nodeContentProperty, null, false, Collections.emptySet())).thenReturn(
                List.of(VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("One", "en")),
                        VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("Jedna", "cs")))
        );
        final ReferencedListIterator<MultilingualString> sut = new ReferencedListIterator<>(descriptor, connector, VF);

        final Axiom<MultilingualString> result = sut.nextAxiom();
        assertNotNull(result);
        assertEquals(new MultilingualString(Map.of("en", "One", "cs", "Jedna")), result.getValue().getValue());
    }

    @Test
    void currentContentReturnsMultilingualStringWhenNodeContentIsMultilingualString() throws Exception {
        final IRI node = VF.createIRI(Generator.generateUri().toString());
        final Statement nextStatement = VF.createStatement(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, node);
        when(connector.findStatements(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, null, false, Collections.emptySet())).thenReturn(List.of(nextStatement));
        when(connector.findStatements(node, ReferencedListHandlerTest.nodeContentProperty, null, false, Collections.emptySet())).thenReturn(
                List.of(VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("One", "en")),
                        VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("Jedna", "cs")))
        );
        final ReferencedListIterator<MultilingualString> sut = new ReferencedListIterator<>(descriptor, connector, VF);

        sut.nextNode();
        final MultilingualString result = sut.currentContent();
        assertEquals(new MultilingualString(Map.of("en", "One", "cs", "Jedna")), result);
    }
}
