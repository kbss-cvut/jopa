package cz.cvut.kbss.jopa.query;

/**
 * Used to parse queries into builders which enable the query to be further manipulated, e.g. set parameters.
 *
 * @author kidney
 */
public interface QueryParser {

    /**
     * Parses the specified query string and returns a query builder instance containing the parsed query.
     *
     * @param query The query to parse
     * @return Query builder with the parsed query
     */
    QueryHolder parseQuery(String query);
}
