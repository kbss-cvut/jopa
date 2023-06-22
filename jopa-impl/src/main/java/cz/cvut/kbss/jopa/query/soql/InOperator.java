package cz.cvut.kbss.jopa.query.soql;

/**
 * SOQL ({@code NOT}) {@code IN} operator.
 */
class InOperator implements FilterableExpression {

    private final boolean isNot;

    private InOperator(boolean isNot) {
        this.isNot = isNot;
    }

    @Override
    public String toFilterExpression(String parameter, String value) {
        return parameter + (isNot ? " " + SoqlConstants.NOT : "") + " " + SoqlConstants.IN + " (" + value + ')';
    }

    static InOperator in() {
        return new InOperator(false);
    }

    static InOperator notIn() {
        return new InOperator(true);
    }
}
