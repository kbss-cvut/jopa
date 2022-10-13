package cz.cvut.kbss.jopa.query;

/**
 * Defines query hints supported by JOPA.
 */
public class QueryHints {

    /**
     * Indicates that inferred results should be omitted from query results.
     */
    public static final String DISABLE_INFERENCE = "cz.cvut.kbss.jopa.query.disableInference";

    private QueryHints() {
        throw new AssertionError();
    }
}
