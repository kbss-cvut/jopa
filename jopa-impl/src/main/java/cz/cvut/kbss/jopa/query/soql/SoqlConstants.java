package cz.cvut.kbss.jopa.query.soql;

/**
 * Constants of the Semantic Object Query Language (SOQL).
 */
public class SoqlConstants {

    /**
     * {@code SELECT} keyword.
     */
    public static final String SELECT = "SELECT";

    /**
     * {@code DISTINCT} operator.
     */
    public static final String DISTINCT = "DISTINCT";

    /**
     * {@code FROM} keyword.
     */
    public static final String FROM = "FROM";

    /**
     * {@code WHERE} keyword.
     */
    public static final String WHERE = "WHERE";

    /**
     * {@code IN} operator.
     */
    public static final String IN = "IN";

    /**
     * {@code NOT} operator.
     */
    public static final String NOT = "NOT";

    /**
     * {@code GROUP BY} statement.
     */
    public static final String GROUP_BY = "GROUP BY";

    /**
     * {@code ORDER BY} statement.
     */
    public static final String ORDER_BY = "ORDER BY";

    /**
     * {@code ASC} keyword.
     */
    public static final String ASC = "ASC";

    /**
     * {@code DESC} keyword.
     */
    public static final String DESC = "DESC";

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

        /**
         * Returns the length of a string.
         */
        public static final String LENGTH = "LENGTH";

        /**
         * Returns the absolute value of the argument.
         */
        public static final String ABS = "ABS";

        /**
         * Returns the smallest number with no fractional part that is not less than the argument.
         */
        public static final String CEIL = "CEIL";

        /**
         * Returns the largest number with no fractional part that is not greater than the argument.
         */
        public static final String FLOOR = "FLOOR";

        private Functions() {
            throw new AssertionError();
        }
    }

    private SoqlConstants() {
        throw new AssertionError();
    }
}
