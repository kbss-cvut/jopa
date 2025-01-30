package cz.cvut.kbss.ontodriver.owlapi.container;

import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ContainerValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.environment.Generator;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.semanticweb.owlapi.io.StringDocumentSource;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.net.URI;
import java.util.List;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ContainerHandlerTest {

    private final NamedResource owner = NamedResource.create(Generator.generateUri());

    private OntologySnapshot ontologySnapshot;
    private OWLOntology ontology;
    private OWLDataFactory dataFactory;

    @Mock
    private OwlapiAdapter owlapiAdapter;

    private ContainerHandler sut;

    @BeforeEach
    void setUp() throws Exception {
        this.ontologySnapshot = TestUtils.initRealOntology(null);
        this.ontology = ontologySnapshot.getOntology();
        this.dataFactory = ontologySnapshot.getDataFactory();
        this.sut = new ContainerHandler(owlapiAdapter, ontologySnapshot);
    }

    @Test
    void readContainerLoadsContainerElementsAndReturnsAxiomsWithThem() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_2 "2"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_3 "3"^^xsd:int .
                """.formatted(owner.toString(), property.getIdentifier());
        StringDocumentSource source = new StringDocumentSource(ttl);
        OWLOntology loaded = ontologySnapshot.getOntologyManager().loadOntologyFromOntologyDocument(source);
        ontologySnapshot.getOntologyManager().addAxioms(ontology, loaded.axioms());

        final List<Axiom<?>> result = sut.readContainer(ContainerDescriptor.seqDescriptor(owner, property));
        assertNotNull(result);
        assertEquals(3, result.size());
        for (int i = 0; i < result.size(); i++) {
            assertEquals(owner, result.get(i).getSubject());
            assertEquals(property, result.get(i).getAssertion());
            assertEquals(new Value<>(i + 1), result.get(i).getValue());
        }
    }

    @Test
    void readContainerHandlesContainerRepresentedByAnonymousIndividual() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> [
                 rdf:_1 "1"^^xsd:int .
                ] .""".formatted(owner.toString(), property.getIdentifier());
        StringDocumentSource source = new StringDocumentSource(ttl);
        OWLOntology loaded = ontologySnapshot.getOntologyManager().loadOntologyFromOntologyDocument(source);
        ontologySnapshot.getOntologyManager().addAxioms(ontology, loaded.axioms());

        final List<Axiom<?>> result = sut.readContainer(ContainerDescriptor.seqDescriptor(owner, property));
        assertNotNull(result);
        assertEquals(1, result.size());
        for (int i = 0; i < result.size(); i++) {
            assertEquals(owner, result.get(i).getSubject());
            assertEquals(property, result.get(i).getAssertion());
            assertEquals(new Value<>(i + 1), result.get(i).getValue());
        }
    }

    @Test
    void readContainerSupportsIndividualsAsContainerMembers() throws Exception {
        final Assertion property = Assertion.createObjectPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                @prefix owl: <http://www.w3.org/2002/07/owl#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 <http://example.com/read_uncommitted> .
                <https://example.com/hasIsolationLevels/container> rdf:_2 <http://example.com/read_committed> .
                rdf:_1 a owl:ObjectProperty .
                rdf:_2 a owl:ObjectProperty .
                """.formatted(owner.toString(), property.getIdentifier());
        StringDocumentSource source = new StringDocumentSource(ttl);
        OWLOntology loaded = ontologySnapshot.getOntologyManager().loadOntologyFromOntologyDocument(source);
        ontologySnapshot.getOntologyManager().addAxioms(ontology, loaded.axioms());

        final List<Axiom<?>> result = sut.readContainer(ContainerDescriptor.seqDescriptor(owner, property));
        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals(NamedResource.create("http://example.com/read_uncommitted"), result.get(0).getValue().getValue());
        assertEquals(NamedResource.create("http://example.com/read_committed"), result.get(1).getValue().getValue());
    }

    @Test
    void readContainerThrowsIntegrityConstraintViolatedExceptionWhenContainerValuesAreNotUnique() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "2"^^xsd:int .
                <%s> <%s> <https://example.com/hasIsolationLevels/anotherObject> .
                """.formatted(owner.toString(), property.getIdentifier(), owner.toString(), property.getIdentifier());
        StringDocumentSource source = new StringDocumentSource(ttl);
        OWLOntology loaded = ontologySnapshot.getOntologyManager().loadOntologyFromOntologyDocument(source);
        ontologySnapshot.getOntologyManager().addAxioms(ontology, loaded.axioms());

        assertThrows(IntegrityConstraintViolatedException.class, () -> sut.readContainer(ContainerDescriptor.seqDescriptor(owner, property)));
    }

    @Test
    void loadContainerPreservesOrderBasedOnContainerMembershipPropertiesNumbering() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_3 "3"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_2 "2"^^xsd:int .
                """.formatted(owner.toString(), property.getIdentifier());
        StringDocumentSource source = new StringDocumentSource(ttl);
        OWLOntology loaded = ontologySnapshot.getOntologyManager().loadOntologyFromOntologyDocument(source);
        ontologySnapshot.getOntologyManager().addAxioms(ontology, loaded.axioms());

        final List<Axiom<?>> result = sut.readContainer(ContainerDescriptor.seqDescriptor(owner, property));
        assertNotNull(result);
        assertEquals(3, result.size());
        for (int i = 0; i < result.size(); i++) {
            assertEquals(owner, result.get(i).getSubject());
            assertEquals(property, result.get(i).getAssertion());
            assertEquals(new Value<>(i + 1), result.get(i).getValue());
        }
    }

    @Test
    void persistContainerCreatesContainerAndAddsSpecifiedValuesToIt() {
        final Assertion property = Assertion.createObjectPropertyAssertion(URI.create("https://example.com/hasCandidates"), false);
        final List<NamedResource> values = List.of(NamedResource.create(Generator.generateUri()), NamedResource.create(Generator.generateUri()));
        final ContainerValueDescriptor<NamedResource> descriptor = ContainerValueDescriptor.bagValueDescriptor(owner, property);
        values.forEach(descriptor::addValue);
        when(owlapiAdapter.generateIdentifier(any())).thenReturn(Generator.generateUri());

        sut.persistContainer(descriptor);
        verifyContainerContent(property, RDF.BAG, values);
    }

    private void verifyContainerContent(Assertion property, String containerType, List<?> values) {
        final OWLNamedIndividual ownerIndividual = dataFactory.getOWLNamedIndividual(IRI.create(owner.getIdentifier()));
        final OWLObjectProperty containerProperty = dataFactory.getOWLObjectProperty(IRI.create(property.getIdentifier()));
        final List<OWLIndividual> container = EntitySearcher.getObjectPropertyValues(ownerIndividual, containerProperty, ontology)
                                                            .toList();
        assertEquals(1, container.size());
        assertTrue(container.get(0).isNamed());
        final OWLNamedIndividual containerIndividual = (OWLNamedIndividual) container.get(0);
        assertTrue(EntitySearcher.containsAxiom(dataFactory.getOWLClassAssertionAxiom(dataFactory.getOWLClass(IRI.create(containerType)), containerIndividual), ontology, Imports.INCLUDED));
        for (int i = 0; i < values.size(); i++) {
            final OWLAxiom axiom = switch (property.getType()) {
                case OBJECT_PROPERTY ->
                        dataFactory.getOWLObjectPropertyAssertionAxiom(dataFactory.getOWLObjectProperty(IRI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#_" + (i + 1))), containerIndividual, dataFactory.getOWLNamedIndividual(IRI.create(values.get(i)
                                                                                                                                                                                                                                                       .toString())));
                case DATA_PROPERTY ->
                        dataFactory.getOWLDataPropertyAssertionAxiom(dataFactory.getOWLDataProperty(IRI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#_" + (i + 1))), containerIndividual, OwlapiUtils.createOWLLiteralFromValue(values.get(i), property.getLanguage()));
                case ANNOTATION_PROPERTY, CLASS, PROPERTY ->
                        throw new UnsupportedOperationException("RDF containers of property type " + property.getType() + " are not supported.");
            };
            assertTrue(EntitySearcher.containsAxiom(axiom, ontology, Imports.INCLUDED));
        }
    }

    @Test
    void persistContainerDoesNothingWhenValuesAreEmpty() {
        final Assertion property = Assertion.createObjectPropertyAssertion(URI.create("https://example.com/hasCandidates"), false);
        final ContainerValueDescriptor<NamedResource> descriptor = ContainerValueDescriptor.bagValueDescriptor(owner, property);

        sut.persistContainer(descriptor);
        assertTrue(ontology.isEmpty());
    }

    @Test
    void updateContainerAppendsItemsToExistingContent() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                @prefix owl: <http://www.w3.org/2002/07/owl#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> a rdf:Seq .
                <%s> a owl:ObjectProperty .
                """.formatted(owner.toString(), property.getIdentifier(), property.getIdentifier());
        StringDocumentSource source = new StringDocumentSource(ttl);
        OWLOntology loaded = ontologySnapshot.getOntologyManager().loadOntologyFromOntologyDocument(source);
        ontologySnapshot.getOntologyManager().addAxioms(ontology, loaded.axioms());

        final ContainerValueDescriptor<Integer> update = ContainerValueDescriptor.seqValueDescriptor(owner, property);
        IntStream.range(1, 5).forEach(update::addValue);

        sut.updateContainer(update);
        verifyContainerContent(property, RDF.SEQ, update.getValues());
    }

    @Test
    void updateContainerRemovesAndAddsElementsToExistingContainer() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                @prefix owl: <http://www.w3.org/2002/07/owl#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_2 "2"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_3 "3"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> a rdf:Seq .
                <%s> a owl:ObjectProperty .
                """.formatted(owner.toString(), property.getIdentifier(), property.getIdentifier());
        StringDocumentSource source = new StringDocumentSource(ttl);
        OWLOntology loaded = ontologySnapshot.getOntologyManager().loadOntologyFromOntologyDocument(source);
        ontologySnapshot.getOntologyManager().addAxioms(ontology, loaded.axioms());

        final ContainerValueDescriptor<Integer> descriptor = ContainerValueDescriptor.seqValueDescriptor(owner, property);
        IntStream.range(1, 10).filter(i -> i % 2 == 0).forEach(descriptor::addValue);
        sut.updateContainer(descriptor);
        verifyContainerContent(property, RDF.SEQ, descriptor.getValues());
    }

    @Test
    void updateContainerRemovesContainerAndItsContentWhenValuesAreEmpty() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                @prefix owl: <http://www.w3.org/2002/07/owl#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_2 "2"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_3 "3"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> a rdf:Seq .
                <%s> a owl:ObjectProperty .
                """.formatted(owner.toString(), property.getIdentifier(), property.getIdentifier());
        StringDocumentSource source = new StringDocumentSource(ttl);
        OWLOntology loaded = ontologySnapshot.getOntologyManager().loadOntologyFromOntologyDocument(source);
        ontologySnapshot.getOntologyManager().addAxioms(ontology, loaded.axioms());

        sut.updateContainer(ContainerValueDescriptor.seqValueDescriptor(owner, property));
        assertTrue(ontology.getABoxAxioms(Imports.INCLUDED).isEmpty());
    }
}
