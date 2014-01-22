package cz.cvut.kbss.ontodriver.impl.sesame;

import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.TupleQueryResult;

import cz.cvut.kbss.ontodriver.AbstractStatement;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exceptions.QueryExecutionException;

class SesameStatement extends AbstractStatement {

	private StorageProxy storage;

	public SesameStatement(JopaStatement statement) {
		super(statement);
	}

	@Override
	public ResultSet executeStatement() throws QueryExecutionException {
		assert storage != null;
		try {

			final TupleQueryResult tqr = storage.executeQuery(query);
			return new SesameResultSet(tqr, jopaStatement);
		} catch (QueryEvaluationException e) {
			throw new QueryExecutionException("Exception caught when evaluating query " + query, e);
		}
	}

	void setStorage(StorageProxy storage) {
		assert storage != null;
		this.storage = storage;
	}
}
