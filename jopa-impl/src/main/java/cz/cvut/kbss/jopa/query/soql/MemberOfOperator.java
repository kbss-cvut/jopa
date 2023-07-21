package cz.cvut.kbss.jopa.query.soql;

/**
 * SOQL ({@code NOT}) {@code MEMBER OF} operator.
 */
public class MemberOfOperator implements FilterableExpression {

    private final boolean isNot;

    private MemberOfOperator(boolean isNot) {
        this.isNot = isNot;
    }

    @Override
    public String toFilterExpression(String parameter, String value) {
        // TODO FILTER NOT EXISTS
        return "";
    }

    @Override
    public boolean requiresFilterExpression() {
        return isNot;
    }

    static MemberOfOperator memberOf() {
        return new MemberOfOperator(false);
    }

    static MemberOfOperator notMemberOf() {
        return new MemberOfOperator(true);
    }
}
