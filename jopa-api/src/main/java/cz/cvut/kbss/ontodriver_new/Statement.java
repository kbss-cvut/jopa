package cz.cvut.kbss.ontodriver_new;

import java.net.URI;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * This interface represents a SPARQL statement.
 * 
 * @author kidney
 * 
 */
public interface Statement extends AutoCloseable, cz.cvut.kbss.ontodriver.Statement {

	/**
	 * Execute the specified SPARQL query.
	 * 
	 * @param sparql
	 *            The statement to execute
	 * @param contexts
	 *            Specifies contexts against which to run the query. Since this
	 *            parameter is optional, it is defined as varargs.
	 * @return {@code ResultSet} containing results of the query
	 * @throws OntoDriverException
	 *             If an error occurs during query execution
	 */
	public ResultSet executeQuery(String sparql, URI... contexts) throws OntoDriverException;

	/**
	 * Execute the specified SPARQL update query. </p>
	 * 
	 * The return value is optional and implementations may choose to return 0
	 * by default.
	 * 
	 * @param sparql
	 *            The statement to execute
	 * @param contexts
	 *            Specifies contexts against which to run the query. Since this
	 *            parameter is optional, it is defined as varargs.
	 * @throws OntoDriverException
	 *             If an error occurs during query execution
	 */
	public void executeUpdate(String sparql, URI... contexts) throws OntoDriverException;

	/**
	 * Use the transactional ontology for query processing. </p>
	 * 
	 * Using the transactional ontology can produce different results than using
	 * the central (backup) ontology, since the transactional ontology can
	 * contain uncommitted changes from the current transaction. </p>
	 * 
	 * This is default behavior.
	 * 
	 * @see #setUseBackupOntology()
	 */
	public void setUseTransactionalOntology();

	/**
	 * Returns true if the transactional ontology should be used for query
	 * processing.
	 * 
	 * @return boolean
	 */
	public boolean useTransactionalOntology();

	/**
	 * Use the backup (central) ontology for query processing.
	 * 
	 * @see #useTransactionalOntology()
	 */
	public void setUseBackupOntology();

	/**
	 * Returns true if the backup (central) ontology should be used for query
	 * processing.
	 * 
	 * @return boolean
	 */
	public boolean useBackupOntology();
}
