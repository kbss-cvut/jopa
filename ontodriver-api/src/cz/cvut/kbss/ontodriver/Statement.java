package cz.cvut.kbss.ontodriver;

/**
 * This interface represents a SPARQL statement.
 * 
 * @author kidney
 * 
 */
public interface Statement {

	/**
	 * Execute the specified SPARQL query.
	 * 
	 * @param sparqlQuery
	 *            The query to execute
	 * @return {@code ResultSet} containing results of the query
	 * @throws OntoDriverException
	 *             If an error occurs during query execution
	 */
	public ResultSet executeQuery(String sparqlQuery)
			throws OntoDriverException;

	/**
	 * Execute the specified SPARQL update query. </p>
	 * 
	 * The return value is optional and implementations may choose to return 0
	 * by default.
	 * 
	 * @param sparqlQuery
	 *            The query to execute
	 * @return Number of affected axioms
	 * @throws OntoDriverException
	 *             If an error occurs during query execution
	 */
	public int executeUpdate(String sparqlQuery) throws OntoDriverException;
}
