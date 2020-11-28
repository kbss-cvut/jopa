package cz.cvut.kbss.jopa.model.metamodel;

/**
 * Instances of the type SingularQueryAttribute represents persistent single-valued
 * properties or fields defined by a query.
 *
 * @param <X>
 *            The type containing the represented attribute
 * @param <T>
 *            The type of the represented attribute
 */
public interface SingularQueryAttribute<X, T> extends QueryAttribute<X, T> {

    /**
     * Return the type that represents the type of the attribute.
     *
     * @return type of attribute
     */
    Type<T> getType();
}
