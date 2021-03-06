package cz.cvut.kbss.jopa.model.query.criteria;

import java.util.List;

public interface Predicate extends Expression<Boolean>{

    /**
     * Return the top-level conjuncts or disjuncts of the predicate. Returns empty list if there are no top-level conjuncts or disjuncts of the predicate. Modifications to the list do not affect the query.
     * @return list of boolean expressions forming the predicate
     */
    List<Expression<Boolean>> getExpressions();

    /**
     * Return the boolean operator for the predicate. If the predicate is simple, this is AND.
     * @return boolean operator for the predicate
     */
    Predicate.BooleanOperator getOperator();

    /**
     * Create a negation of the predicate.
     * @return negated predicate
     */
    Predicate not();

    /**
     * Determines if the predicate has been created from another predicate by applying the Predicate.not() method.
     * @return boolean indicating if the predicate is a negated predicate
     */
    boolean isNegated();

    public static enum BooleanOperator{
        AND("AND"),
        OR("OR");

        private final String booleanOperator;

        BooleanOperator(String booleanOperator) {
            this.booleanOperator = booleanOperator;
        }

        @Override
        public String toString() {
            return booleanOperator;
        }
    }
}
