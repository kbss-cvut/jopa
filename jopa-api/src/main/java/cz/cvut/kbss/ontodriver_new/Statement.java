package cz.cvut.kbss.ontodriver_new;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

import java.net.URI;

/**
 * This interface represents a SPARQL statement.
 *
 * @author kidney
 */
public interface Statement extends AutoCloseable, cz.cvut.kbss.ontodriver.Statement {

    /**
     * Specifies which ontology is used for statement evaluation.
     */
    enum StatementOntology {
        /**
         * Transactional ontology. May contain uncommitted changes which influence the statement evaluation.
         */
        TRANSACTIONAL,
        /**
         * The main ontology in the current state. No uncommitted changes are present in it.
         */
        CENTRAL
    }

    /**
     * Execute the specified SPARQL query.
     *
     * @param sparql   The statement to execute
     * @param contexts Specifies contexts against which to run the query. Since this parameter is optional, it is
     *                 defined as varargs.
     * @return {@code ResultSet} containing results of the query
     * @throws OntoDriverException If an error occurs during query execution
     */
    ResultSet executeQuery(String sparql, URI... contexts) throws OntoDriverException;

    /**
     * Execute the specified SPARQL update query. </p>
     * <p>
     * The return value is optional and implementations may choose to return 0 by default.
     *
     * @param sparql   The statement to execute
     * @param contexts Specifies contexts against which to run the query. Since this parameter is optional, it is
     *                 defined as varargs.
     * @throws OntoDriverException If an error occurs during query execution
     */
    void executeUpdate(String sparql, URI... contexts) throws OntoDriverException;

    /**
     * Sets which ontology is used to evaluate this statement.
     * <p>
     * {@link cz.cvut.kbss.ontodriver_new.Statement.StatementOntology#TRANSACTIONAL} ontology is the transactional
     * snapshot. It may contain uncommitted changes and thus the query results may differ from evaluation against {@link
     * cz.cvut.kbss.ontodriver_new.Statement.StatementOntology#CENTRAL}.
     *
     * @param ontology Which ontology to use
     */
    void useOntology(StatementOntology ontology);

    /**
     * Gets information about which ontology will be used to evaluate the statement.
     *
     * @return Which ontology will be used for evaluation
     * @see #useOntology(StatementOntology)
     */
    StatementOntology getStatementOntology();
}
