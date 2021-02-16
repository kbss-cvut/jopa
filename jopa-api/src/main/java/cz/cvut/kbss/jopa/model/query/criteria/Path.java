package cz.cvut.kbss.jopa.model.query.criteria;

// TODO PRO - Javadoc
public interface Path<X> extends Expression<X>{

    /**
     * Create a path corresponding to the referenced attribute.
     * @param attributeName name of the attribute
     * @param <Y>
     * @return path corresponding to the referenced attribute
     * @throws IllegalArgumentException if attribute of the given name does not otherwise exist
     */
    <Y> Path<Y> getAttr(String attributeName) throws IllegalArgumentException;

    Path<?> getParentPath();

    Expression<Class<? extends X>> type();
}
