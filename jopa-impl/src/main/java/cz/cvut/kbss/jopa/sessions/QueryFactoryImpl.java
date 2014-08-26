package cz.cvut.kbss.jopa.sessions;

import java.util.List;
import java.util.Objects;

import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.owlapi.QueryImpl;
import cz.cvut.kbss.jopa.owlapi.TypedQueryImpl;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

class QueryFactoryImpl implements QueryFactory {

	private final UnitOfWork uow;
	private final ConnectionWrapper connection;

	QueryFactoryImpl(UnitOfWork uow, ConnectionWrapper connection) {
		assert uow != null;
		assert connection != null;
		this.uow = uow;
		this.connection = connection;
	}

	@Override
	public Query<List<String>> createNativeQuery(String sparql) {
		Objects.requireNonNull(sparql, ErrorUtils.constructNPXMessage("sparql"));

		final QueryImpl q = new QueryImpl(sparql, true, connection);
		q.setUseBackupOntology(uow.useBackupOntologyForQueryProcessing());
		return q;
	}

	@Override
	public <T> TypedQuery<T> createNativeQuery(String sparql, Class<T> resultClass) {
		Objects.requireNonNull(sparql, ErrorUtils.constructNPXMessage("sparql"));
		Objects.requireNonNull(resultClass, ErrorUtils.constructNPXMessage("resultClass"));

		final TypedQueryImpl<T> tq = new TypedQueryImpl<T>(sparql, resultClass, true, uow,
				connection);
		tq.setUseBackupOntology(uow.useBackupOntologyForQueryProcessing());
		return tq;
	}

	@Override
	public Query createQuery(String query) {
		Objects.requireNonNull(query, ErrorUtils.constructNPXMessage("query"));

		// We specify the query as SPARQL since currently we don't support any
		// more abstract syntax
		final QueryImpl q = new QueryImpl(query, false, connection);
		q.setUseBackupOntology(uow.useBackupOntologyForQueryProcessing());
		return q;
	}

	@Override
	public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass) {
		Objects.requireNonNull(query, ErrorUtils.constructNPXMessage("query"));
		Objects.requireNonNull(resultClass, ErrorUtils.constructNPXMessage("resultClass"));

		final TypedQueryImpl<T> tq = new TypedQueryImpl<T>(query, resultClass, false, uow,
				connection);
		tq.setUseBackupOntology(uow.useBackupOntologyForQueryProcessing());
		return tq;
	}
}
