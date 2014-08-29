package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * This interface extends the {@code Statement} and adds the possibility to
 * parameterize queries. </p>
 * 
 * Implementations are also expected to support at least a basic level of
 * character escaping (e. g. quotes) and other injection-protection methods.
 * 
 * @author kidney
 * 
 */
public interface PreparedStatement {

	/**
	 * Executes query represented by this statement. </p>
	 * 
	 * @return ResultSet of the query
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 */
	public ResultSet executeQuery() throws OntoDriverException;

	/**
	 * Executes an update represented by this statement.
	 * 
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 */
	public void executeUpdate() throws OntoDriverException;

	/**
	 * Sets query parameter at the specified index.
	 * 
	 * @param parameterIndex
	 *            Parameter index, the first parameter has index 0
	 * @param value
	 *            The value of the parameter
	 * @throws OntoDriverException
	 *             If the {@code parameterIndex} is not valid or some other
	 *             error occurs
	 */
	public void setObject(int parameterIndex, Object value) throws OntoDriverException;
}
