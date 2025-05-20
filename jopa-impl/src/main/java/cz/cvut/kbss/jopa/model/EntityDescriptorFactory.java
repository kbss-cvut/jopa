package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

/**
 * Creates entity descriptors based on metamodel information.
 * <p>
 * The logic is based on the following rules:
 * <ul>
 *     <li>If a class has {@link cz.cvut.kbss.jopa.model.annotations.Context} declared, use it for all its fields</li>
 *     <li>If a field has {@link cz.cvut.kbss.jopa.model.annotations.Context} declared, and it is an object property referencing another entity, propagate the context to that entity</li>
 *     <li>If a class does not have {@link cz.cvut.kbss.jopa.model.annotations.Context} declared, use context propagated to its from referencing class (if available)</li>
 *     <li>If a field has {@link cz.cvut.kbss.jopa.model.annotations.Context} and is not an object property referencing another entity, use the context for the value</li>
 * </ul>
 */
public interface EntityDescriptorFactory {

    /**
     * Creates a descriptor for the specified entity class.
     *
     * @param cls Entity class to generate descriptor for
     * @param <T> Entity type
     * @return Entity descriptor
     * @throws IllegalArgumentException If the specified class is not an entity class
     */
    <T> Descriptor createDescriptor(Class<T> cls);
}
