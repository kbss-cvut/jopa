package cz.cvut.kbss.jopa.model.metamodel;

/**
 * Instances of the type CollectionAttribute represent persistent
 * javax.util.Collection-valued attributes defined by a query.
 *
 * @param <X>
 *            The type the represented Collection belongs to
 * @param <E>
 *            The element type of the represented Collection
 */
public interface CollectionQueryAttribute<X, E> extends PluralQueryAttribute<X, java.util.Collection<E>, E> {
}
