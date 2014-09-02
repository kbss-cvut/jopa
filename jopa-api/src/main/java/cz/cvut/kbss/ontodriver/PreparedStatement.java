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
	 * Sets value of binding with the specified name.
	 * 
	 * @param binding
	 *            Binding name
	 * @param value
	 *            The value of the parameter
	 * @return This statement
	 * @throws OntoDriverException
	 *             If there is no such binding in the statement or some other
	 *             error occurs
	 * @throws IllegalStateException
	 *             If called on a closed statement
	 */
	public void setObject(String binding, Object value) throws OntoDriverException;
}
