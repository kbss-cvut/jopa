package cz.cvut.kbss.ontodriver.sesame.connector;

import org.openrdf.query.TupleQueryResult;

public interface StatementExecutor {

	/**
	 * Executes the specified query and returns result in form of a Sesame query
	 * result.
	 * 
	 * @param query
	 *            The query to execute
	 * @return Tuple query result
	 */
	public TupleQueryResult executeQuery(String query);

	/**
	 * Executes the specified update query.
	 * 
	 * @param query
	 *            The query to execute
	 */
	public void executeUpdate(String query);
}
