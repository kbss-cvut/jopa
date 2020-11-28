package cz.cvut.kbss.jopa.model.metamodel;

/**
 * Instances of the type SetAttribute represent persistent java.util.Set-valued
 * attributes defined by a query.
 *
 * @param <X>
 *            The type the represented Set belongs to
 * @param <E>
 *            The element type of the represented Set
 */
public interface SetQueryAttribute<X, E> extends PluralAttribute<X, java.util.Set<E>, E> {
}
