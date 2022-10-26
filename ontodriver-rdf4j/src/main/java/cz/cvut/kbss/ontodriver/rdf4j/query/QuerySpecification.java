package cz.cvut.kbss.ontodriver.rdf4j.query;

import java.util.Objects;

/**
 * Represents a query and configuration relevant to the query execution.
 */
public class QuerySpecification {

    private final String query;

    private boolean includeInference = true;

    private QuerySpecification(String query) {
        this.query = Objects.requireNonNull(query);
    }

    public String getQuery() {
        return query;
    }

    public QuerySpecification includeInference(boolean includeInference) {
        this.includeInference = includeInference;
        return this;
    }

    public boolean isIncludeInference() {
        return includeInference;
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
        return includeInference == that.includeInference && query.equals(that.query);
    }

    @Override
    public int hashCode() {
        return Objects.hash(query, includeInference);
    }

    public static QuerySpecification query(String query) {
        return new QuerySpecification(query);
    }
}
