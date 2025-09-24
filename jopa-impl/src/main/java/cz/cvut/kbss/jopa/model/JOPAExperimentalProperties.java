package cz.cvut.kbss.jopa.model;

/**
 * Experimental configuration properties.
 * <p>
 * These properties may be removed or changed in future versions without changing major version number.
 */
public class JOPAExperimentalProperties {

    /**
     * Enables the use of {@link cz.cvut.kbss.jopa.query.sparql.EntityLoadingOptimizer} for SPARQL queries.
     */
    public static final String QUERY_ENABLE_ENTITY_LOADING_OPTIMIZER = "cz.cvut.kbss.jopa.experimental.query.enableEntityLoadingOptimizer";
}
