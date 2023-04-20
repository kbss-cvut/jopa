package cz.cvut.kbss.jopa.query.soql;

/**
 * Constants of the Semantic Object Query Language (SOQL).
 */
public class SoqlConstants {

    /**
     * {@code DISTINCT} operator.
     */
    public static final String DISTINCT = "DISTINCT";

    /**
     * {@code IN} operator.
     */
    public static final String IN = "IN";

    /**
     * {@code NOT IN} operator.
     */
    public static final String NOT_IN = "NOT IN";

    /**
     * {@code NOT} operator.
     */
    public static final String NOT = "NOT";

    /**
     * SPARQL shortcut for {@code rdf:type} - {@code a}.
     */
    public static final String RDF_TYPE = "a";

    /**
     * SOQL variable prefix.
     * <p>
     * Same as in JPQL.
     */
    public static final char VARIABLE_PREFIX = ':';

    /**
     * Supported SOQL functions.
     */
    public static class Functions {

        /**
         * Takes an argument string and transforms it to lower case.
         */
        public static final String LOWER = "LOWER";

        /**
         * Takes an argument string and transforms it to upper case.
         */
        public static final String UPPER = "UPPER";

        /**
         * Counts the number of elements.
         */
        public static final String COUNT = "COUNT";

        private Functions() {
            throw new AssertionError();
        }
    }

    private SoqlConstants() {
        throw new AssertionError();
    }
}
