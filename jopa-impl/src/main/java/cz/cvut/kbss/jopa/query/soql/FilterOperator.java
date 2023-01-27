package cz.cvut.kbss.jopa.query.soql;

/**
 * Operator for SPARQL FILTER expressions.
 */
interface FilterOperator {

    /**
     * Generates a SPARQL filter expression for the specified first and second argument.
     *
     * @param parameter Variable already declared in the query
     * @param value     Value (variable) used to filter the query-bound variable values
     * @return SPARQL FILTER expression
     */
    String toFilterExpression(String parameter, String value);

    /**
     * Whether this operator does require a SPARQL FILTER.
     * <p>
     * For example, equality operator does not require a FILTER because a simple triple pattern is functionally
     * equivalent and has to be used anyway.
     *
     * @return Boolean specifying whether this operator is required
     */
    default boolean requiresFilterExpression() {
        return true;
    }
}
