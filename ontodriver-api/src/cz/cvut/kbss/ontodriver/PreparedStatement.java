package cz.cvut.kbss.ontodriver;

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
public interface PreparedStatement extends Statement {

	/**
	 * Set query parameter at the specified index.
	 * 
	 * @param parameterIndex
	 *            Parameter index, the first parameter has index 0
	 * @param value
	 *            The value of the parameter
	 * @return This statement
	 * @throws OntoDriverException
	 *             If the {@code parameterIndex} is not valid or some other
	 *             error occurs
	 */
	public PreparedStatement setParameter(int parameterIndex, Object value)
			throws OntoDriverException;

	/**
	 * Set query parameter with the specified value.
	 * 
	 * @param parameterLabel
	 *            Label of the parameter
	 * @param value
	 *            The value of the parameter
	 * @return This statement
	 * @throws OntoDriverException
	 *             If the {@code parameterLabel} is not valid or some other
	 *             error occurs
	 */
	public PreparedStatement setParameter(String parameterLabel, Object value)
			throws OntoDriverException;
}
