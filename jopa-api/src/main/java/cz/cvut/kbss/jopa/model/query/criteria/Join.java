package cz.cvut.kbss.jopa.model.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;

public interface Join<Z, X> extends From<Z, X> {

    /**
     * Gets the metamodel attribute corresponding to the join.
     *
     * @return metamodel attribute corresponding to the join
     */
    Attribute<? super Z, ?> getAttribute();

    /**
     * Gets the join type.
     *
     * @return Join type
     */
    JoinType getJoinType();

    /**
     * Gets the predicate that corresponds to the {@literal ON} restriction(s) on the join, or {@code nul} if no ON
     * condition has been specified.
     *
     * @return the ON restriction predicate
     */
    Predicate getOn();

    /**
     * Gets the parent of the join.
     *
     * @return Join parent
     */
    From<?, Z> getParent();

    /**
     * Modify the join to restrict the result according to the specified ON condition and return the join object.
     * <p>
     * Replaces the previous ON condition, if any.
     *
     * @param restriction A simple or compound boolean expression
     * @return The modified join object
     */
    Join<Z, X> on(Expression<Boolean> restriction);

    /**
     * Modify the join to restrict the result according to the specified ON condition and return the join object.
     * <p>
     * Replaces the previous ON condition, if any.
     *
     * @param restrictions Zero or more restriction predicates
     * @return The modified join object
     */
    Join<Z, X> on(Predicate... restrictions);
}
