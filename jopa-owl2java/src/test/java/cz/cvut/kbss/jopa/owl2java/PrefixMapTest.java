package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.owl2java.config.Defaults;
import cz.cvut.kbss.jopa.owl2java.config.TransformationConfiguration;
import cz.cvut.kbss.jopa.owl2java.environment.Generator;
import cz.cvut.kbss.jopa.owl2java.exception.OWL2JavaException;
import cz.cvut.kbss.jopa.vocabulary.DC;
import cz.cvut.kbss.jopa.vocabulary.OWL;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import cz.cvut.kbss.jopa.vocabulary.SKOS;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PrefixMapTest {

    private final OWLDataFactory dataFactory = new OWLDataFactoryImpl();

    private final OWLOntologyManager ontologyManager = OWLManager.createOWLOntologyManager();

    @Test
    void getPrefixReturnsPrefixResolvedFromSingleStringAnnotationPropertyValue() {
        final TransformationConfiguration config = new TransformationConfiguration.TransformationConfigurationBuilder().build();
        final String prefix = "owl2java";
        final IRI ontologyIri = IRI.create(Generator.generateUri());
        assertOntologyPrefixAnnotation(prefix, ontologyIri);
        final PrefixMap sut = new PrefixMap(ontologyManager, config);
        final Optional<String> result = sut.getPrefix(ontologyIri);
        assertTrue(result.isPresent());
        assertEquals(prefix, result.get());
    }

    @Test
    void getPrefixReturnsPrefixResolvedFromSingleStringDataPropertyValue() throws Exception {
        final TransformationConfiguration config = new TransformationConfiguration.TransformationConfigurationBuilder().build();
        final String prefix = "owl2java";
        final IRI ontologyIri = IRI.create(Generator.generateUri());
        final OWLOntology ontology = ontologyManager.createOntology(ontologyIri);
        final OWLDataProperty prefixProperty = dataFactory.getOWLDataProperty(Defaults.ONTOLOGY_PREFIX_PROPERTY);
        ontology.add(dataFactory.getOWLDataPropertyAssertionAxiom(prefixProperty, dataFactory.getOWLNamedIndividual(ontologyIri), dataFactory.getOWLLiteral(prefix)));
        final PrefixMap sut = new PrefixMap(ontologyManager, config);
        final Optional<String> result = sut.getPrefix(ontologyIri);
        assertTrue(result.isPresent());
        assertEquals(prefix, result.get());
    }

    @Test
    void getPrefixReturnsPrefixesOfMultipleOntologies() {
        final TransformationConfiguration config = new TransformationConfiguration.TransformationConfigurationBuilder().build();
        final Map<String, IRI> prefixes = Map.of(
                "owl2java", IRI.create(Generator.generateUri()),
                "jopa", IRI.create(Generator.generateUri())
        );
        prefixes.forEach(this::assertOntologyPrefixAnnotation);
        final PrefixMap sut = new PrefixMap(ontologyManager, config);

        prefixes.forEach((prefix, iri) -> {
            final Optional<String> result = sut.getPrefix(iri);
            assertTrue(result.isPresent());
            assertEquals(prefix, result.get());
        });
    }

    @Test
    void getPrefixReturnsPrefixWhenMultiplePrefixAssertionsAreInMergedOntology() {
        final TransformationConfiguration config = new TransformationConfiguration.TransformationConfigurationBuilder().build();
        final Map<String, IRI> prefixes = Map.of(
                "owl2java", IRI.create(Generator.generateUri()),
                "jopa", IRI.create(Generator.generateUri())
        );
        final OWLOntology ontology = assertOntologyPrefixAnnotation("owl2java", prefixes.get("owl2java"));
        final OWLAnnotationProperty prefixProperty = dataFactory.getOWLAnnotationProperty(Defaults.ONTOLOGY_PREFIX_PROPERTY);
        ontology.add(dataFactory.getOWLAnnotationAssertionAxiom(prefixProperty, prefixes.get("jopa"), dataFactory.getOWLLiteral("jopa")));

        final PrefixMap sut = new PrefixMap(ontologyManager, config);
        prefixes.forEach((prefix, iri) -> {
            final Optional<String> result = sut.getPrefix(iri);
            assertTrue(result.isPresent());
            assertEquals(prefix, result.get());
        });
    }

    private OWLOntology assertOntologyPrefixAnnotation(String prefix, IRI ontologyIri) {
        try {
            final OWLOntology ontology = ontologyManager.createOntology(ontologyIri);
            final OWLAnnotationProperty prefixProperty = dataFactory.getOWLAnnotationProperty(Defaults.ONTOLOGY_PREFIX_PROPERTY);
            ontology.add(dataFactory.getOWLAnnotationAssertionAxiom(prefixProperty, ontologyIri, dataFactory.getOWLLiteral(prefix)));
            return ontology;
        } catch (OWLOntologyCreationException e) {
            throw new RuntimeException(e);
        }
    }

    @ParameterizedTest
    @MethodSource("predefinedPrefixes")
    void getPrefixReturnsPredefinedPrefixesForSelectedVocabularies(String expectedPrefix, IRI iri) {
        final TransformationConfiguration config = new TransformationConfiguration.TransformationConfigurationBuilder().build();
        final String prefix = "owl2java";
        final IRI ontologyIri = IRI.create(Generator.generateUri());
        assertOntologyPrefixAnnotation(prefix, ontologyIri);
        final PrefixMap sut = new PrefixMap(ontologyManager, config);
        final Optional<String> result = sut.getPrefix(iri);
        assertTrue(result.isPresent());
        assertEquals(expectedPrefix, result.get());
    }

    static Stream<Arguments> predefinedPrefixes() {
        return Stream.of(
                Arguments.of("dcterms", IRI.create(DC.Terms.NAMESPACE)),
                Arguments.of(RDF.PREFIX, IRI.create(RDF.NAMESPACE)),
                Arguments.of(RDFS.PREFIX, IRI.create(RDFS.NAMESPACE)),
                Arguments.of(OWL.PREFIX, IRI.create(OWL.NAMESPACE)),
                Arguments.of(SKOS.PREFIX, IRI.create(SKOS.NAMESPACE)),
                Arguments.of("foaf", IRI.create("http://xmlns.com/foaf/0.1/"))
        );
    }

    @Test
    void getPrefixReturnsPrefixResolvedFromPrefixMappingFileSpecifiedInConfiguration() throws Exception {
        final String prefixMappingFilePath = TestUtils.resolveTestResourcesFilePath("prefixMappingFile");
        final TransformationConfiguration config = new TransformationConfiguration.TransformationConfigurationBuilder().prefixMappingFile(prefixMappingFilePath)
                                                                                                                       .build();
        final PrefixMap sut = new PrefixMap(ontologyManager, config);
        final Optional<String> siocPrefix = sut.getPrefix(IRI.create("http://rdfs.org/sioc/ns#"));
        assertTrue(siocPrefix.isPresent());
        assertEquals("sioc", siocPrefix.get());
        final Optional<String> ddoPrefix = sut.getPrefix(IRI.create("http://onto.fel.cvut.cz/ontologies/dataset-descriptor/"));
        assertTrue(ddoPrefix.isPresent());
        assertEquals("ddo", ddoPrefix.get());
    }

    @Test
    void prefixResolvingThrowsOWL2JavaExceptionWhenPrefixMappingFileIsNotFound() {
        final TransformationConfiguration config = new TransformationConfiguration.TransformationConfigurationBuilder().prefixMappingFile("unknownFile")
                                                                                                                       .build();
        assertThrows(OWL2JavaException.class, () -> new PrefixMap(ontologyManager, config));
    }
}
