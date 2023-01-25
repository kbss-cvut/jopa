package cz.cvut.kbss.jopa.query.soql;

/**
 * SPARQL {@code IN} operator.
 */
class InOperator implements FilterOperator {

    @Override
    public String toFilterExpression(String parameter, String value) {
        return parameter + " " + SoqlConstants.IN + " (" + value + ')';
    }
}
