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

import cz.cvut.kbss.jopa.owl2java.config.TransformationConfiguration;
import cz.cvut.kbss.jopa.owl2java.exception.OWL2JavaException;
import cz.cvut.kbss.jopa.vocabulary.DC;
import cz.cvut.kbss.jopa.vocabulary.OWL;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import cz.cvut.kbss.jopa.vocabulary.SKOS;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.search.EntitySearcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Keeps a map of prefixes resolved from provided ontologies.
 * <p>
 * The following prefix sources are used:
 * <ul>
 *     <li>An ontology may specify a prefix using the configured prefix property. If the ontology specifies a namespace, it is used, otherwise the ontology IRI is used as the namespace</li>
 *     <li>Remove prefix resolver may be configured</li>
 *     <li>A prefix mapping file may be configured</li>
 * </ul>
 */
public class PrefixMap {

    private static final Logger LOG = LoggerFactory.getLogger(PrefixMap.class);

    /**
     * Map ontology IRI -> prefix
     */
    private final Map<String, String> prefixes;

    public PrefixMap(OWLOntologyManager ontologyManager, TransformationConfiguration config) {
        this.prefixes = resolvePrefixes(Objects.requireNonNull(ontologyManager), config);
    }

    /**
     * Resolves prefixes of ontologies accessible from the specified ontology manager.
     *
     * @param ontologyManager Manager of ontologies to process
     */
    private static Map<String, String> resolvePrefixes(OWLOntologyManager ontologyManager,
                                                       TransformationConfiguration config) {
        final Map<String, String> result = new ConcurrentHashMap<>(builtInPrefixes());
        ontologyManager.ontologies().parallel().filter(o -> o.getOntologyID().isNamed()).forEach(o -> {
            assert o.getOntologyID().getOntologyIRI().isPresent();
            result.putAll(resolveOntologyPrefixes(o, config.getOntologyPrefixProperty(), config.getOntologyNamespaceProperty(), config.getRemotePrefixResolver()));
        });
        result.putAll(resolvePrefixesFromPrefixMappingFile(config.getPrefixMappingFile()));
        LOG.info("Resolved prefix map: {}", result);
        return result;
    }

    private static Map<String, String> resolveOntologyPrefixes(OWLOntology ontology, String prefixProperty,
                                                               String namespaceProperty,
                                                               RemotePrefixResolver remotePrefixResolver) {
        final Map<String, String> result = new HashMap<>();
        assert ontology.getOntologyID().getOntologyIRI().isPresent();
        final IRI ontologyIri = ontology.getOntologyID().getOntologyIRI().get();
        final Optional<String> prefix = resolveOntologyAnnotationValues(ontology, prefixProperty);
        if (prefix.isPresent()) {
            final Optional<String> namespace = resolveOntologyAnnotationValues(ontology, namespaceProperty);
            if (namespace.isPresent()) {
                LOG.debug("Registering prefix {} for namespace {}.", prefix.get(), namespace.get());
                result.put(namespace.get(), prefix.get());
            }
            // Register prefix both for namespace and ontology IRI
            LOG.debug("Registering prefix {} for ontology {}.", prefix.get(), ontologyIri.getIRIString());
            result.put(ontologyIri.getIRIString(), prefix.get());
        } else {
            remotePrefixResolver.resolvePrefix(ontologyIri).ifPresent(p -> result.put(ontologyIri.getIRIString(), p));
        }
        return result;
    }

    private static Optional<String> resolveOntologyAnnotationValues(OWLOntology ontology, String property) {
        final OWLDataFactory df = ontology.getOWLOntologyManager().getOWLDataFactory();
        assert ontology.getOntologyID().getOntologyIRI().isPresent();
        final Optional<String> annotation = ontology.annotations(df.getOWLAnnotationProperty(property))
                                                    .filter(ax -> ax.getValue().isLiteral())
                                                    .map(ax -> ax.getValue().asLiteral().get().getLiteral()).findAny();
        if (annotation.isPresent()) {
            return annotation;
        }
        final Optional<String> annStr = EntitySearcher.getAnnotationAssertionAxioms(ontology.getOntologyID()
                                                                                            .getOntologyIRI()
                                                                                            .get(), ontology)
                                                      .filter(ax -> ax.getAnnotation().getProperty().getIRI().toString()
                                                                      .equals(property) && ax.getValue().isLiteral())
                                                      .map(ax -> ax.getValue().asLiteral().get().getLiteral())
                                                      .findAny();
        if (annStr.isPresent()) {
            return annStr;
        }
        return EntitySearcher.getDataPropertyValues(df.getOWLNamedIndividual(ontology.getOntologyID().getOntologyIRI()
                                                                                     .get()), df.getOWLDataProperty(property), ontology)
                             .map(OWLLiteral::getLiteral).findAny();
    }

    private static Map<String, String> resolvePrefixesFromPrefixMappingFile(String mappingFilePath) {
        if (mappingFilePath == null) {
            return Collections.emptyMap();
        }
        final File file = new File(mappingFilePath);
        try {
            LOG.info("Loading prefix mapping from file '{}'.", file);
            final List<String> lines = Files.readAllLines(file.toPath(), StandardCharsets.UTF_8);
            final Map<String, String> prefixMap = new HashMap<>(lines.size());
            lines.stream().filter(line -> !line.isBlank()).forEach(line -> {
                final String[] mapping = line.split("=");
                if (mapping.length != 2) {
                    LOG.warn("Mapping line '{}' does not correspond to the expected pattern '$namespace=$prefix'. Skipping it.", line);
                }
                prefixMap.put(mapping[0], mapping[1]);
            });
            return prefixMap;
        } catch (IOException e) {
            LOG.error("Unable to read prefix mapping file.", e);
            throw new OWL2JavaException("Unable to read prefix mapping file.", e);
        }
    }

    /**
     * Gets prefix for the specified namespace or ontology IRI.
     *
     * @param namespace Namespace IRI or ontology IRI
     * @return Resolved prefix, if available
     */
    public Optional<String> getNamespacePrefix(IRI namespace) {
        Objects.requireNonNull(namespace);
        return Optional.ofNullable(prefixes.get(namespace.getIRIString()));
    }

    /**
     * Gets prefix for a resource with the specified IRI.
     * <p>
     * Primarily, this means extracting the namespace from the IRI and looking up the prefix for it. If no exact match
     * is found, prefix with the longest matching namespace is returned.
     *
     * @param iri Resource IRI
     * @return Resolved prefix, if available
     */
    public Optional<String> getPrefix(IRI iri) {
        Objects.requireNonNull(iri);
        final String namespace = iri.getNamespace();
        final Optional<String> prefix = getNamespacePrefix(IRI.create(namespace));
        if (prefix.isPresent()) {
            return prefix;
        }
        final String strIri = iri.getIRIString();
        return prefixes.keySet().stream().filter(strIri::startsWith).max(Comparator.comparingInt(String::length))
                       .map(prefixes::get);
    }

    /**
     * Checks whether a prefix has been resolved for an ontology with the specified IRI.
     *
     * @param ontologyIri Ontology IRI
     * @return {@code true} if a prefix is registered for the ontology IRI, {@code false} otherwise
     */
    public boolean hasOntologyPrefix(IRI ontologyIri) {
        Objects.requireNonNull(ontologyIri);
        return prefixes.containsKey(ontologyIri.getIRIString());
    }

    /**
     * Checks whether a prefix is available for the specified IRI.
     * <p>
     * The prefix may be exact match for the namespace or the longest matching namespace for the IRI.
     *
     * @param iri IRI to check
     * @return {@code true} if a prefix is registered for the IRI, {@code false} otherwise
     */
    public boolean hasPrefix(IRI iri) {
        final String strIri = iri.getIRIString();
        return prefixes.containsKey(iri.getNamespace()) || prefixes.keySet().stream().anyMatch(strIri::startsWith);
    }

    private static Map<String, String> builtInPrefixes() {
        return Map.of(
                RDF.NAMESPACE, RDF.PREFIX,
                RDFS.NAMESPACE, RDFS.PREFIX,
                OWL.NAMESPACE, OWL.PREFIX,
                SKOS.NAMESPACE, SKOS.PREFIX,
                DC.Terms.NAMESPACE, "dcterms",
                DC.Elements.NAMESPACE, "dc",
                "http://xmlns.com/foaf/0.1/", "foaf"
        );
    }
}
