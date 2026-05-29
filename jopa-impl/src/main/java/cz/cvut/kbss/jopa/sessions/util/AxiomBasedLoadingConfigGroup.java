package cz.cvut.kbss.jopa.sessions.util;

import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.net.URI;

/**
 * Helper configuration for loading entities directly from axioms.
 *
 * @param subject    (Root) entity identifier
 * @param descriptor Entity data descriptor
 * @param fetchGraph Fetch graph, possibly {@code null}
 */
public record AxiomBasedLoadingConfigGroup<T>(URI subject, Descriptor descriptor, EntityGraph<T> fetchGraph) {

    public AxiomBasedLoadingConfigGroup(URI subject, Descriptor descriptor) {
        this(subject, descriptor, null);
    }
}
