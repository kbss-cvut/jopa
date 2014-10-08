package cz.cvut.kbss.ontodriver.sesame.connector;

import java.util.List;

import org.openrdf.query.BindingSet;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.TupleQueryResult;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;

/**
 * This class wraps the Sesame TupleQueryResult returned by QueryExecutor to be
 * able to close the repository connection once the result is closed.
 * 
 * @author ledvima1
 * 
 */
class QueryResult implements TupleQueryResult {

	private final TupleQueryResult result;
	private final RepositoryConnection connection;

	QueryResult(TupleQueryResult result, RepositoryConnection connection) {
		this.result = result;
		this.connection = connection;
	}

	@Override
	public void close() throws QueryEvaluationException {
		result.close();
		try {
			connection.close();
		} catch (RepositoryException e) {
			throw new QueryEvaluationException(e);
		}
	}

	@Override
	public boolean hasNext() throws QueryEvaluationException {
		return result.hasNext();
	}

	@Override
	public BindingSet next() throws QueryEvaluationException {
		return result.next();
	}

	@Override
	public void remove() throws QueryEvaluationException {
		result.remove();
	}

	@Override
	public List<String> getBindingNames() throws QueryEvaluationException {
		return result.getBindingNames();
	}
}
