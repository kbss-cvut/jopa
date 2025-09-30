package cz.cvut.kbss.jopa.sessions.util;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.util.Collection;

/**
 * Loading parameters for loading an entity from axioms.
 *
 * @param cls         Entity class
 * @param descriptor  Entity descriptor
 * @param bypassCache Whether to bypass cache
 * @param axioms      Axioms to load the entity from
 * @param <T>         Entity type
 */
public record AxiomBasedLoadingParameters<T>(Class<T> cls, Descriptor descriptor, boolean bypassCache,
                                             Collection<Axiom<?>> axioms) {
}
