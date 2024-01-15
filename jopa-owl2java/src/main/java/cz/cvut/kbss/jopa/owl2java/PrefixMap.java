package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.owl2java.config.TransformationConfiguration;
import cz.cvut.kbss.jopa.owl2java.exception.OWL2JavaException;
import cz.cvut.kbss.jopa.vocabulary.DC;
import cz.cvut.kbss.jopa.vocabulary.OWL;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import cz.cvut.kbss.jopa.vocabulary.SKOS;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * Keeps a map of prefixes resolved from provided ontologies.
 * <p>
 * Prefixes are resolved using the following strategy:
 * <ul>
 *     <li>If an ontology has an explicitly specified prefix (using the configured prefix property), it is used</li>
 *     <li></li>
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
    private Map<String, String> resolvePrefixes(OWLOntologyManager ontologyManager, TransformationConfiguration config) {
        final Map<String, String> result = new HashMap<>(defaultPrefixes());
        ontologyManager.ontologies().filter(o -> o.getOntologyID().isNamed()).forEach(o -> {
            assert o.getOntologyID().getOntologyIRI().isPresent();
            result.putAll(resolveOntologyPrefixes(o, config.getOntologyPrefixProperty()));
        });
        result.putAll(resolvePrefixesFromPrefixMappingFile(config.getPrefixMappingFile()));
        LOG.debug("Resolved prefix map: {}", result);
        return result;
    }

    private Map<String, String> resolveOntologyPrefixes(OWLOntology ontology, String prefixProperty) {
        final Map<String, String> result = new HashMap<>();
        final OWLDataFactory df = ontology.getOWLOntologyManager().getOWLDataFactory();
        final OWLAnnotationProperty annProperty = df.getOWLAnnotationProperty(prefixProperty);
        assert ontology.getOntologyID().getOntologyIRI().isPresent();
        ontology.axioms(AxiomType.ANNOTATION_ASSERTION)
                .filter(ax -> ax.getProperty().equals(annProperty) && ax.getValue().isLiteral() && ax.getSubject()
                                                                                                     .isIRI())
                .forEach(ax -> result.put(ax.getSubject().asIRI().get().getIRIString(), ax.getValue().asLiteral().get()
                                                                                          .getLiteral()));
        final OWLDataProperty dataProperty = df.getOWLDataProperty(prefixProperty);
        ontology.axioms(AxiomType.DATA_PROPERTY_ASSERTION)
                .filter(ax -> ax.getProperty().equals(dataProperty) && ax.getSubject().isIndividual())
                .forEach(ax -> result.put(ax.getSubject().asOWLNamedIndividual().toStringID(), ax.getObject()
                                                                                                 .getLiteral()));
        return result;
    }

    private Map<String, String> resolvePrefixesFromPrefixMappingFile(String mappingFilePath) {
        if (mappingFilePath == null) {
            return Collections.emptyMap();
        }
        final File file = new File(mappingFilePath);
        try {
            LOG.debug("Loading prefix mapping from file '{}'.", file);
            final List<String> lines = Files.readAllLines(file.toPath(), StandardCharsets.UTF_8);
            final Map<String, String> prefixMap = new HashMap<>(lines.size());
            lines.forEach(line -> {
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
     * Gets prefix for an ontology with the specified IRI.
     *
     * @param ontologyIri Ontology IRI
     * @return Resolved prefix, if available
     */
    public Optional<String> getPrefix(IRI ontologyIri) {
        Objects.requireNonNull(ontologyIri);
        return Optional.ofNullable(prefixes.get(ontologyIri.getIRIString()));
    }

    private static Map<String, String> defaultPrefixes() {
        return Map.of(
                RDF.NAMESPACE, RDF.PREFIX,
                RDFS.NAMESPACE, RDFS.PREFIX,
                OWL.NAMESPACE, OWL.PREFIX,
                SKOS.NAMESPACE, SKOS.PREFIX,
                DC.Terms.NAMESPACE, "dcterms",
                "http://xmlns.com/foaf/0.1/", "foaf"
        );
    }
}
