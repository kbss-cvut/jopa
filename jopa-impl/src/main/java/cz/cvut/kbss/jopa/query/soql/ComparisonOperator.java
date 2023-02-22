package cz.cvut.kbss.jopa.query.soql;

/**
 * Comparison operator, e.g., &lt;, &gt;.
 */
class ComparisonOperator implements FilterOperator {

    private final String operator;

    public ComparisonOperator(String operator) {
        assert operator != null;
        this.operator = operator;
    }

    @Override
    public String toFilterExpression(String parameter, String value) {
        return parameter + " " + translateOperator() + " " + value;
    }

    private String translateOperator() {
        return "<>".equals(operator) ? "!=" : operator;
    }

    @Override
    public boolean requiresFilterExpression() {
        return !"=".equals(operator);
    }
}
