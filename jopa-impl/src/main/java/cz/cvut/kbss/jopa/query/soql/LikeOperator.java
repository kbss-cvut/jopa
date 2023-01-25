package cz.cvut.kbss.jopa.query.soql;

class LikeOperator implements FilterOperator {

    @Override
    public String toFilterExpression(String parameter, String value) {
        return "regex(" + parameter + ", " + value + ") ";
    }
}
