package cz.cvut.kbss.ontodriver.impl.sesame;

import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;

final class SesameRemoveChange implements SesameChange {

	private final Statement statement;
	private final URI context;

	SesameRemoveChange(Statement statement, URI context) {
		assert statement != null;

		this.statement = statement;
		this.context = context;
	}

	@Override
	public void apply(RepositoryConnection connection) throws RepositoryException {
		assert connection != null;
		connection.remove(statement, context);
	}

}
