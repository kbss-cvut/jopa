package cz.cvut.kbss.jopa.model.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.CollectionAttribute;
import cz.cvut.kbss.jopa.model.metamodel.SetAttribute;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;

import java.util.Set;

/**
 * Represents a bound type, usually an entity that appears in the {@literal FROM} clause.
 * <p>
 * Serves as a factory for {@literal JOIN}s of associations and collections belonging to the type, and for Paths of
 * attributes belonging to the type.
 *
 * @param <Z> The source type
 * @param <X> The target type
 */
public interface From<Z, X> extends Path<X> {

    /**
     * Return the joins that have been made from this bound type.
     * <p>
     * Returns empty set if no joins have been made from this bound type. Modifications to the set do not affect the
     * query.
     *
     * @return Joins made from this type
     */
    Set<Join<X, ?>> getJoins();

    /**
     * Create an inner join to the specified Collection-valued attribute.
     *
     * @param collection Target of the join
     * @param <Y>        Collection element type
     * @return The resulting join
     */
    <Y> Join<X, Y> join(CollectionAttribute<? super X, Y> collection);

    /**
     * Create a join to the specified Collection-valued attribute using the given join type.
     *
     * @param collection Target of the join
     * @param jt         Join type
     * @param <Y>        Collection element type
     * @return The resulting join
     */
    <Y> Join<X, Y> join(CollectionAttribute<? super X, Y> collection, JoinType jt);

    /**
     * Create an inner join to the specified Set-valued attribute.
     *
     * @param set Target of the join
     * @param <Y> Set element type
     * @return The resulting join
     */
    <Y> Join<X, Y> join(SetAttribute<? super X, Y> set);

    /**
     * Create a join to the specified Set-valued attribute using the given join type.
     *
     * @param set Target of the join
     * @param jt  Join type
     * @param <Y> Set element type
     * @return The resulting join
     */
    <Y> Join<X, Y> join(SetAttribute<? super X, Y> set, JoinType jt);

    /**
     * Create an inner join to the specified single-valued attribute.
     *
     * @param attribute Target of the join
     * @param <Y>       Attribute value type
     * @return The resulting join
     */
    <Y> Join<X, Y> join(SingularAttribute<? super X, Y> attribute);

    /**
     * Create a join to the specified single-valued attribute using the given join type.
     *
     * @param attribute Target of the join
     * @param jt        Join type
     * @param <Y>       Attribute value type
     * @return The resulting join
     */
    <Y> Join<X, Y> join(SingularAttribute<? super X, Y> attribute, JoinType jt);

    /**
     * Create an inner join to the specified attribute.
     *
     * @param attributeName Name of the attribute for the target of the join
     * @param <Y>           Attribute value type
     * @return The resulting join
     * @throws IllegalArgumentException If attribute of the given name does not exist
     */
    <X, Y> Join<X, Y> join(String attributeName);

    /**
     * Create a join to the specified attribute of the given join type.
     *
     * @param attributeName Name of the attribute for the target of the join
     * @param jt            Join type
     * @param <Y>           Attribute value type
     * @return The resulting join
     * @throws IllegalArgumentException If attribute of the given name does not exist
     */
    <X, Y> Join<X, Y> join(String attributeName, JoinType jt);
}
