package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
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
	public Query<List<String>> createNativeQuery(String sparql, URI contextUri) {
		if (sparql == null) {
			throw new NullPointerException("Query not specified!");
		}
		if (contextUri == null) {
			throw new NullPointerException("Context URI not specified.");
		}
		final Query<List<String>> q = new QueryImpl(sparql, contextUri, true, connection);
		((QueryImpl) q).setUseBackupOntology(uow.useBackupOntologyForQueryProcessing());
		return q;

	}

	@Override
	public <T> TypedQuery<T> createNativeQuery(String sparql, Class<T> resultClass, URI contextUri) {
		if (sparql == null || resultClass == null) {
			throw new NullPointerException("Query or resultClass not specified!");
		}
		if (contextUri == null) {
			throw new NullPointerException("Context URI not specified.");
		}
		final TypedQueryImpl<T> tq = new TypedQueryImpl<T>(sparql, resultClass, contextUri, true,
				uow, connection);
		return tq;
	}

	@Override
	public Query createQuery(String query, URI contextUri) {
		if (query == null) {
			throw new NullPointerException("Query not specified!");
		}
		if (contextUri == null) {
			throw new NullPointerException("Context URI not specified.");
		}
		// We specify the query as SPARQL since currently we don't support any
		// more abstract syntax
		final QueryImpl q = new QueryImpl(query, contextUri, false, connection);
		q.setUseBackupOntology(uow.useBackupOntologyForQueryProcessing());
		return q;
	}

	@Override
	public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass, URI contextUri) {
		if (query == null || resultClass == null) {
			throw new NullPointerException("Query or resultClass not specified!");
		}
		if (contextUri == null) {
			throw new NullPointerException("Context URI not specified.");
		}
		final TypedQueryImpl<T> tq = new TypedQueryImpl<T>(query, resultClass, contextUri, false,
				uow, connection);
		return tq;
	}

}
