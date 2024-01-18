package cz.cvut.kbss.jopa.owl2java.prefix;

import org.semanticweb.owlapi.model.IRI;

import java.util.Optional;

/**
 * Attempts to resolve ontology prefix by invoking a remote service.
 */
public interface RemotePrefixResolver {

    /**
     * Resolves prefix of an ontology with the specified IRI.
     *
     * @param ontologyIri IRI of the ontology to get prefix for
     * @return Resolved prefix wrapped in {@code Optional}, empty {@code Optional} if unable to get the prefix
     */
    Optional<String> resolvePrefix(IRI ontologyIri);
}
