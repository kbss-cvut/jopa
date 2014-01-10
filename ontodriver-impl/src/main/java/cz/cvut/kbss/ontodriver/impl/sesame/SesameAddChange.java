package cz.cvut.kbss.ontodriver.impl.sesame;

import org.openrdf.model.Statement;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;

final class SesameAddChange implements SesameChange {

	private final Statement statement;

	SesameAddChange(Statement statement) {
		assert statement != null;
		this.statement = statement;
	}

	@Override
	public void apply(RepositoryConnection connection) throws RepositoryException {
		assert connection != null;
		connection.add(statement);
	}
}
