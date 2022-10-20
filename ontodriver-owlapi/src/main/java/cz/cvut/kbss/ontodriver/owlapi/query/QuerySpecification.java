package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.Statement;

import java.util.Objects;

class QuerySpecification {

    private final String query;

    private Statement statement;

    private boolean disableInference = false;

    private QuerySpecification(String query) {
        this.query = query;
    }

    public String getQuery() {
        return query;
    }

    public Statement getStatement() {
        return statement;
    }

    public boolean isDisableInference() {
        return disableInference;
    }

    public QuerySpecification statement(Statement statement) {
        this.statement = statement;
        return this;
    }

    public QuerySpecification disableInference(boolean disableInference) {
        this.disableInference = disableInference;
        return this;
    }

    public static QuerySpecification query(String query) {
        return new QuerySpecification(query);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        QuerySpecification that = (QuerySpecification) o;
        return disableInference == that.disableInference && query.equals(that.query);
    }

    @Override
    public int hashCode() {
        return Objects.hash(query, disableInference);
    }
}
