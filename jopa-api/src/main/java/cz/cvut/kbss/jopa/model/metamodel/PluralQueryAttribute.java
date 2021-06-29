package cz.cvut.kbss.jopa.model.metamodel;

/**
 * Instances of the type PluralAttribute represent persistent collection-valued
 * attributes defined by a query.
 *
 * @param <X>
 *            The type the represented collection belongs to
 * @param <C>
 *            The type of the represented collection
 * @param <E>
 *            The element type of the represented collection
 */
public interface PluralQueryAttribute<X, C, E> extends QueryAttribute<X, C>, Bindable<E> {

    /**
     * Return the collection type.
     *
     * @return collection type
     */
    CollectionType getCollectionType();

    /**
     * Return the type representing the element type of the collection.
     *
     * @return element type
     */
    Type<E> getElementType();
}
