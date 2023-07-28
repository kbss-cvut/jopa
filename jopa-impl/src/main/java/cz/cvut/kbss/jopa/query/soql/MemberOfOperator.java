package cz.cvut.kbss.jopa.query.soql;

/**
 * SOQL ({@code NOT}) {@code MEMBER OF} operator.
 */
public class MemberOfOperator implements FilterableExpression {

    @Override
    public String toFilterExpression(String parameter, String value) {
        return "";
    }

    @Override
    public boolean requiresFilterExpression() {
        return false;
    }

    static MemberOfOperator memberOf() {
        return new MemberOfOperator();
    }
}
