package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ContainerValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.util.Collection;

/**
 * Interface for managing RDF containers.
 */
public interface Containers {

    /**
     * Reads values from an RDF container specified by the given descriptor.
     *
     * @param descriptor Container descriptor
     * @return Collection of axioms representing the values in the container. The property of the axioms corresponds to
     * the property specified in the container descriptor (i.e., property reference the container from the owner).
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    Collection<Axiom<?>> readContainer(ContainerDescriptor descriptor) throws OntoDriverException;


    /**
     * Persists values to an RDF container specified by the given descriptor.
     *
     * @param descriptor Container descriptor
     * @param <T>        Container value type
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    <T> void persistContainer(ContainerValueDescriptor<T> descriptor) throws OntoDriverException;

    /**
     * Updates values in an RDF container specified by the given descriptor.
     *
     * @param descriptor Container descriptor
     * @param <T>        Container value type
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    <T> void updateContainer(ContainerValueDescriptor<T> descriptor) throws OntoDriverException;
}
