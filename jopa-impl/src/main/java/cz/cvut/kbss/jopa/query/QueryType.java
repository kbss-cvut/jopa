package cz.cvut.kbss.jopa.query;

/**
 * Types of queries.
 */
public enum QueryType {
    SELECT("SELECT"),
    ASK("ASK"),
    DESCRIBE("DESCRIBE"),
    DELETE("DELETE"),
    INSERT("INSERT"),
    CONSTRUCT("CONSTRUCT");

    private final String keyword;

    QueryType(String keyword) {this.keyword = keyword;}

    public String getKeyword() {
        return keyword;
    }
}
