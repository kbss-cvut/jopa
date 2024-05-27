package cz.cvut.kbss.jopa.sessions.cache;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;

/**
 * Descriptors of an entity.
 *
 * @param repositoryDescriptor Repository-related descriptor
 * @param loadStateDescriptor  Load state descriptor
 */
public record Descriptors(Descriptor repositoryDescriptor, LoadStateDescriptor<?> loadStateDescriptor) {
}
