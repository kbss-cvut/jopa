package cz.cvut.kbss.ontodriver.impl.sesame;

import org.openrdf.query.MalformedQueryException;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.QueryLanguage;
import org.openrdf.query.TupleQuery;
import org.openrdf.query.TupleQueryResult;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;

import cz.cvut.kbss.ontodriver.AbstractStatement;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exceptions.QueryExecutionException;

class SesameStatement extends AbstractStatement {

	private RepositoryConnection conn;

	public SesameStatement(JopaStatement statement) {
		super(statement);
	}

	@Override
	public ResultSet executeStatement() throws QueryExecutionException {
		assert conn != null;
		try {
			final TupleQuery tq = conn.prepareTupleQuery(QueryLanguage.SPARQL, query);

			final TupleQueryResult tqr = tq.evaluate();
			return new SesameResultSet(conn, tqr, jopaStatement);
		} catch (QueryEvaluationException e) {
			throw new QueryExecutionException("Exception caught when evaluating query " + query, e);
		} catch (RepositoryException e) {
			throw new QueryExecutionException("Exception caught when preparing query " + query, e);
		} catch (MalformedQueryException e) {
			throw new QueryExecutionException(
					"Malformed query exception caught for query " + query, e);
		}
	}

	void setConnection(RepositoryConnection connection) {
		assert connection != null;

		this.conn = connection;
	}
}
