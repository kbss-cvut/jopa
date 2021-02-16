package cz.cvut.kbss.jopa.model.query.criteria;

//TODO PRO - predicate interface - methods
public interface Predicate extends Expression<Boolean>{

    Predicate.BooleanOperator getOperator();

    /**
     * Create a conjunction of the given boolean expressions.
     * @param x boolean expression
     * @param y boolean expression
     * @return and predicate
     */
    Predicate and(Expression<Boolean> x, Expression<Boolean> y);

    /**
     * Create a conjunction of the given restriction predicates. A conjunction of zero predicates is true.
     * @param restrictions zero or more restriction predicates
     * @return and predicate
     */
    Predicate and(Predicate... restrictions);

    /**
     * Create a disjunction of the given boolean expressions.
     * @param x boolean expression
     * @param y boolean expression
     * @return or predicate
     */
    Predicate or(Expression<Boolean> x, Expression<Boolean> y);

    /**
     * Create a disjunction of the given restriction predicates. A disjunction of zero predicates is false.
     * @param restrictions zero or more restriction predicates
     * @return or predicate
     */
    Predicate or(Predicate... restrictions);

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
