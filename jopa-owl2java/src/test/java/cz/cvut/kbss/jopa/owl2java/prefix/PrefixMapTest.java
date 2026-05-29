/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.owl2java.prefix;

import cz.cvut.kbss.jopa.owl2java.TestUtils;
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
import org.semanticweb.owlapi.formats.TurtleDocumentFormat;
import org.semanticweb.owlapi.io.StreamDocumentSource;
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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class PrefixMapTest {

    private final OWLDataFactory dataFactory = new OWLDataFactoryImpl();

    private final OWLOntologyManager ontologyManager = OWLManager.createOWLOntologyManager();

    private final RemotePrefixResolver remotePrefixResolver = mock(RemotePrefixResolver.class);

    @Test
    void getNamespacePrefixReturnsOntologyPrefixResolvedFromSingleStringAnnotationPropertyValue() {
        final TransformationConfiguration config = configBuilder().build();
        final String prefix = "owl2java";
        final IRI ontologyIri = IRI.create(Generator.generateUri());
        assertOntologyPrefixAnnotation(prefix, ontologyIri);
        final PrefixMap sut = new PrefixMap(ontologyManager, config);
        final Optional<String> result = sut.getNamespacePrefix(ontologyIri);
        assertTrue(result.isPresent());
        assertEquals(prefix, result.get());
    }

    private TransformationConfiguration.TransformationConfigurationBuilder configBuilder() {
        return new TransformationConfiguration.TransformationConfigurationBuilder().remotePrefixResolver(remotePrefixResolver);
    }

    @Test
    void getNamespacePrefixReturnsOntologyPrefixResolvedFromSingleStringDataPropertyValue() throws Exception {
        final TransformationConfiguration config = configBuilder().build();
        final String prefix = "owl2java";
        final IRI ontologyIri = IRI.create(Generator.generateUri());
        final OWLOntology ontology = ontologyManager.createOntology(ontologyIri);
        final OWLDataProperty prefixProperty = dataFactory.getOWLDataProperty(Defaults.ONTOLOGY_PREFIX_PROPERTY);
        ontology.add(dataFactory.getOWLDataPropertyAssertionAxiom(prefixProperty, dataFactory.getOWLNamedIndividual(ontologyIri), dataFactory.getOWLLiteral(prefix)));
        final PrefixMap sut = new PrefixMap(ontologyManager, config);
        final Optional<String> result = sut.getNamespacePrefix(ontologyIri);
        assertTrue(result.isPresent());
        assertEquals(prefix, result.get());
    }

    @Test
    void getNamespacePrefixReturnsPrefixesOfMultipleOntologies() {
        final TransformationConfiguration config = configBuilder().build();
        final Map<String, IRI> prefixes = Map.of(
                "owl2java", IRI.create(Generator.generateUri()),
                "jopa", IRI.create(Generator.generateUri())
        );
        prefixes.forEach(this::assertOntologyPrefixAnnotation);
        final PrefixMap sut = new PrefixMap(ontologyManager, config);

        prefixes.forEach((prefix, iri) -> {
            final Optional<String> result = sut.getNamespacePrefix(iri);
            assertTrue(result.isPresent());
            assertEquals(prefix, result.get());
        });
    }

    private void assertOntologyPrefixAnnotation(String prefix, IRI ontologyIri) {
        try {
            final OWLOntology ontology = ontologyManager.createOntology(ontologyIri);
            final OWLAnnotationProperty prefixProperty = dataFactory.getOWLAnnotationProperty(Defaults.ONTOLOGY_PREFIX_PROPERTY);
            ontology.add(dataFactory.getOWLAnnotationAssertionAxiom(prefixProperty, ontologyIri, dataFactory.getOWLLiteral(prefix)));
        } catch (OWLOntologyCreationException e) {
            throw new RuntimeException(e);
        }
    }

    @ParameterizedTest
    @MethodSource("predefinedPrefixes")
    void getNamespacePrefixReturnsPredefinedPrefixesForSelectedVocabularies(String expectedPrefix, IRI iri) {
        final TransformationConfiguration config = configBuilder().build();
        final String prefix = "owl2java";
        final IRI ontologyIri = IRI.create(Generator.generateUri());
        assertOntologyPrefixAnnotation(prefix, ontologyIri);
        final PrefixMap sut = new PrefixMap(ontologyManager, config);
        final Optional<String> result = sut.getNamespacePrefix(iri);
        assertTrue(result.isPresent());
        assertEquals(expectedPrefix, result.get());
    }

    static Stream<Arguments> predefinedPrefixes() {
        return Stream.of(
                Arguments.of("dcterms", IRI.create(DC.Terms.NAMESPACE)),
                Arguments.of(RDF.PREFIX, IRI.create(RDF.NAMESPACE)),
                Arguments.of(RDFS.PREFIX, IRI.create(RDFS.NAMESPACE)),
                Arguments.of(OWL.PREFIX, IRI.create(OWL.NAMESPACE)),
                Arguments.of(SKOS.PREFIX, IRI.create(SKOS.NAMESPACE))
        );
    }

    @Test
    void getPrefixReturnsPrefixResolvedFromOntologyPrefixMappingFileSpecifiedInConfiguration() {
        final String prefixMappingFilePath = TestUtils.resolveTestResourcesFilePath("prefixMappingFile");
        final TransformationConfiguration config = configBuilder().prefixMappingFile(prefixMappingFilePath).build();
        final PrefixMap sut = new PrefixMap(ontologyManager, config);
        final Optional<String> siocPrefix = sut.getNamespacePrefix(IRI.create("http://rdfs.org/sioc/ns#"));
        assertTrue(siocPrefix.isPresent());
        assertEquals("sioc", siocPrefix.get());
        final Optional<String> ddoPrefix = sut.getNamespacePrefix(IRI.create("http://onto.fel.cvut.cz/ontologies/dataset-descriptor/"));
        assertTrue(ddoPrefix.isPresent());
        assertEquals("ddo", ddoPrefix.get());
    }

    @Test
    void prefixResolvingThrowsOWL2JavaExceptionWhenPrefixMappingFileIsNotFound() {
        final TransformationConfiguration config = configBuilder().prefixMappingFile("unknownFile").build();
        assertThrows(OWL2JavaException.class, () -> new PrefixMap(ontologyManager, config));
    }

    @Test
    void prefixResolvingUsesRemotePrefixResolverWhenOntologyPrefixCannotBeResolvedLocally() throws Exception {
        final TransformationConfiguration config = configBuilder().build();
        final String prefix = "owl2java";
        final IRI ontologyIri = IRI.create(Generator.generateUri());
        ontologyManager.createOntology(ontologyIri);
        when(remotePrefixResolver.resolvePrefix(ontologyIri)).thenReturn(Optional.of(prefix));
        final PrefixMap sut = new PrefixMap(ontologyManager, config);
        final Optional<String> result = sut.getNamespacePrefix(ontologyIri);
        assertTrue(result.isPresent());
        assertEquals(prefix, result.get());
        verify(remotePrefixResolver).resolvePrefix(ontologyIri);
    }

    @Test
    void prefixResolvingUsesPrefixAndNamespaceDeclaredInOntology() throws Exception {
        final TransformationConfiguration config = configBuilder().build();
        ontologyManager.loadOntologyFromOntologyDocument(new StreamDocumentSource(PrefixMapTest.class.getClassLoader()
                                                                                                     .getResourceAsStream("with-prefix.ttl"),
                IRI.create("http://onto.fel.cvut.cz/ontologies/application/termit"), new TurtleDocumentFormat(), "text/turtle"));
        final PrefixMap sut = new PrefixMap(ontologyManager, config);

        final Optional<String> result = sut.getNamespacePrefix(IRI.create("http://onto.fel.cvut.cz/ontologies/application/termit"));
        assertTrue(result.isPresent());
        assertEquals("termit", result.get());
    }

    @Test
    void getPrefixReturnsPrefixResolvedForNamespaceOfResourceWithSlashDelimiter() throws Exception {
        final TransformationConfiguration config = configBuilder().build();
        ontologyManager.loadOntologyFromOntologyDocument(new StreamDocumentSource(PrefixMapTest.class.getClassLoader()
                                                                                                     .getResourceAsStream("with-prefix.ttl"),
                IRI.create("http://onto.fel.cvut.cz/ontologies/application/termit"), new TurtleDocumentFormat(), "text/turtle"));
        final PrefixMap sut = new PrefixMap(ontologyManager, config);

        final Optional<String> result = sut.getPrefix(IRI.create("http://onto.fel.cvut.cz/ontologies/application/termit/term/vocabulary"));
        assertTrue(result.isPresent());
        assertEquals("termit", result.get());
    }

    @Test
    void getPrefixReturnsPrefixResolvedForNamespaceOfResourceWithHashtagDelimiter() {
        final TransformationConfiguration config = configBuilder().build();
        final String prefix = "owl2java";
        final IRI ontologyIri = IRI.create(Generator.generateUri());
        assertOntologyPrefixAnnotation(prefix, ontologyIri);
        final PrefixMap sut = new PrefixMap(ontologyManager, config);

        final Optional<String> result = sut.getPrefix(IRI.create(RDFS.CLASS));
        assertTrue(result.isPresent());
        assertEquals("rdfs", result.get());
    }

    @Test
    void getPrefixAttemptsToMatchLongestRegisteredNamespaceAsResourceIriSubstringForPrefixResolution() {
        final TransformationConfiguration config = configBuilder().build();
        final String prefix = "owl2java";
        final IRI ontologyIri = IRI.create("http://onto.fel.cvut.cz/ontologies/jopa/owl2java");
        assertOntologyPrefixAnnotation(prefix, ontologyIri);
        final PrefixMap sut = new PrefixMap(ontologyManager, config);
        final Optional<String> result = sut.getPrefix(IRI.create("http://onto.fel.cvut.cz/ontologies/jopa/owl2java/term/vocabulary"));
        assertTrue(result.isPresent());
        assertEquals(prefix, result.get());
    }
}
