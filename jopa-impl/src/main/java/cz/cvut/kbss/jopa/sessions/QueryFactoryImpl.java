package cz.cvut.kbss.jopa.sessions;

import java.util.List;

import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.owlapi.QueryImpl;
import cz.cvut.kbss.jopa.owlapi.TypedQueryImpl;
import cz.cvut.kbss.ontodriver.Connection;

class QueryFactoryImpl implements QueryFactory {

	private final UnitOfWork uow;
	private final Connection connection;

	QueryFactoryImpl(UnitOfWork uow, Connection connection) {
		assert uow != null;
		assert connection != null;
		this.uow = uow;
		this.connection = connection;
	}

	@Override
	public Query<List<String>> createNativeQuery(String sparql) {
		if (sparql == null) {
			throw new NullPointerException("Query not specified!");
		}
		final QueryImpl q = new QueryImpl(sparql, true, connection);
		q.setUseBackupOntology(uow.useBackupOntologyForQueryProcessing());
		return q;
	}

	@Override
	public <T> TypedQuery<T> createNativeQuery(String sparql, Class<T> resultClass) {
		if (sparql == null || resultClass == null) {
			throw new NullPointerException("Query or resultClass not specified!");
		}
		final TypedQueryImpl<T> tq = new TypedQueryImpl<T>(sparql, resultClass, true, uow,
				connection);
		return tq;
	}

	@Override
	public Query createQuery(String query) {
		if (query == null) {
			throw new NullPointerException("Query not specified!");
		}
		// We specify the query as SPARQL since currently we don't support any
		// more abstract syntax
		final QueryImpl q = new QueryImpl(query, false, connection);
		q.setUseBackupOntology(uow.useBackupOntologyForQueryProcessing());
		return q;
	}

	@Override
	public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass) {
		if (query == null || resultClass == null) {
			throw new NullPointerException("Query or resultClass not specified!");
		}
		final TypedQueryImpl<T> tq = new TypedQueryImpl<T>(query, resultClass, false, uow,
				connection);
		return tq;
	}

}
