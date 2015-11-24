package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver_new.ResultSet;
import cz.cvut.kbss.ontodriver_new.Statement;

public interface StatementExecutor {

    /**
     * Executes the specified read-only query.
     *
     * @param query     The query to execute
     * @param statement Statement instance representing the query
     * @return Query result
     * @throws OwlapiDriverException
     */
    ResultSet executeQuery(String query, Statement statement) throws OwlapiDriverException;

    /**
     * Executes the specified update query.
     * <p>
     * This query may modify the state of the ontology.
     *
     * @param update The update query to execute
     * @throws OwlapiDriverException
     */
    void executeUpdate(String update) throws OwlapiDriverException;
}
