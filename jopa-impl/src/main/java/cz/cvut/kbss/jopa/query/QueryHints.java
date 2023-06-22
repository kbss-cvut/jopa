package cz.cvut.kbss.jopa.query;

/**
 * Defines query hints supported by JOPA.
 */
public class QueryHints {

    /**
     * Indicates that inferred results should be omitted from query results.
     */
    public static final String DISABLE_INFERENCE = "cz.cvut.kbss.jopa.query.disableInference";

    /**
     * Allows to specify the target ontology for query execution.
     * <p>
     * By target ontology, it is meant either the shared ontology, which does not contain pending transactional changes,
     * or the transactional ontology (w.r.t. the persistence context issuing the query), where transactional changes may
     * influence the query results.
     *
     * Note that OntoDriver implementations may choose to ignore the selection depending on their internal transaction
     * and query execution mechanism.
     *
     * Valid values are {@literal CENTRAL} and {@literal TRANSACTIONAL}.
     */
    public static final String TARGET_ONTOLOGY = "cz.cvut.kbss.jopa.query.targetOntology";

    private QueryHints() {
        throw new AssertionError();
    }
}
