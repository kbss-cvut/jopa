package cz.cvut.kbss.jopa.query.soql;

/**
 * SOQL ({@code NOT}) {@code LIKE} operator.
 */
class LikeOperator implements FilterOperator {

    private final boolean isNot;

    private LikeOperator(boolean isNot) {
        this.isNot = isNot;
    }

    @Override
    public String toFilterExpression(String parameter, String value) {
        return (isNot ? "!" : "") + "regex(" + parameter + ", " + value + ")";
    }

    static LikeOperator like() {
        return new LikeOperator(false);
    }

    static LikeOperator notLike() {
        return new LikeOperator(true);
    }
}
