package cz.cvut.kbss.ontodriver.sesame.connector;

import org.openrdf.query.TupleQueryResult;

import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

public interface StatementExecutor {

	/**
	 * Executes the specified query and returns result in form of a Sesame query
	 * result.
	 * 
	 * @param query
	 *            The query to execute
	 * @return Tuple query result
	 * @throws SesameDriverException
	 *             When things go wrong with query execution
	 */
	public TupleQueryResult executeQuery(String query) throws SesameDriverException;

	/**
	 * Executes the specified update query.
	 * 
	 * @param query
	 *            The query to execute
	 * @throws SesameDriverException
	 *             When things go wrong with query execution
	 */
	public void executeUpdate(String query) throws SesameDriverException;
}
