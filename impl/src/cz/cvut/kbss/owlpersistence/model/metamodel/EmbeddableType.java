package cz.cvut.kbss.owlpersistence.model.metamodel;

import cz.cvut.kbss.owlpersistence.UnusedJPA;

/**
 * Instances of the type EmbeddableType represent embeddable types.
 * 
 * @param <X>
 *            The represented type.
 */
@UnusedJPA
public interface EmbeddableType<X> extends ManagedType<X> {
}
