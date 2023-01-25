package cz.cvut.kbss.jopa.query.soql;

/**
 * SPARQL {@code NOT IN} operator.
 */
public class NotInOperator implements FilterOperator {

    @Override
    public String toFilterExpression(String parameter, String value) {
        return parameter + " " + SoqlConstants.NOT_IN + " (" + value + ')';
    }
}
