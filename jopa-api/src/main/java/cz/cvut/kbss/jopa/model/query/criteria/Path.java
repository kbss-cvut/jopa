package cz.cvut.kbss.jopa.model.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;

public interface Path<X> extends Expression<X> {

    /**
     * Create a path corresponding to the referenced attribute.
     *
     * @param attributeName name of the attribute
     * @return path corresponding to the referenced attribute
     * @throws IllegalArgumentException if attribute of the given name does not otherwise exist
     */
    <Y> Path<Y> getAttr(String attributeName) throws IllegalArgumentException;

    /**
     * Create a path corresponding to the referenced single-valued attribute.
     *
     * @param attribute single-valued attribute
     * @return path corresponding to the referenced attribute
     */
    <Y> Path<Y> getAttr(SingularAttribute<? super X, Y> attribute);

    /**
     * Return the parent "node" in the path or null if no parent.
     *
     * @return parent
     */
    Path<?> getParentPath();
}
