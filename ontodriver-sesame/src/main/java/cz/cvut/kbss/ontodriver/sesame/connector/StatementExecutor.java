package cz.cvut.kbss.ontodriver.sesame.connector;

import org.openrdf.query.TupleQueryResult;

import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

public interface StatementExecutor {

    /**
     * Executes the specified query and returns result in form of a Sesame query
     * result.
     *
     * @param query The query to execute
     * @return Tuple query result
     * @throws SesameDriverException When things go wrong with query execution
     */
    TupleQueryResult executeSelectQuery(String query) throws SesameDriverException;

    /**
     * Executes the specified boolean query.
     * <p>
     * This method is intended mostly for SPARQL ASK queries.
     *
     * @param query The query to execute
     * @return Boolean result of the query
     * @throws SesameDriverException When things go wrong with query execution
     */
    boolean executeBooleanQuery(String query) throws SesameDriverException;

    /**
     * Executes the specified update query.
     *
     * @param query The query to execute
     * @throws SesameDriverException When things go wrong with query execution
     */
    void executeUpdate(String query) throws SesameDriverException;
}
