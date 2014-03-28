package cz.cvut.kbss.jopa.sessions;

import java.util.List;

import cz.cvut.kbss.jopa.model.RepositoryID;
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
	public Query<List<String>> createNativeQuery(String sparql, RepositoryID repository) {
		if (sparql == null) {
			throw new NullPointerException("Query not specified!");
		}
		if (repository == null) {
			throw new NullPointerException("Repository not specified.");
		}
		final Query<List<String>> q = new QueryImpl(sparql, repository, true, connection);
		((QueryImpl) q).setUseBackupOntology(uow.useBackupOntologyForQueryProcessing());
		return q;
	}

	@Override
	public <T> TypedQuery<T> createNativeQuery(String sparql, Class<T> resultClass,
			RepositoryID repository) {
		if (sparql == null || resultClass == null) {
			throw new NullPointerException("Query or resultClass not specified!");
		}
		if (repository == null) {
			throw new NullPointerException("Repository not specified.");
		}
		final TypedQueryImpl<T> tq = new TypedQueryImpl<T>(sparql, resultClass, repository, true,
				uow, connection);
		return tq;
	}

	@Override
	public Query createQuery(String query, RepositoryID repository) {
		if (query == null) {
			throw new NullPointerException("Query not specified!");
		}
		if (repository == null) {
			throw new NullPointerException("Repository not specified.");
		}
		// We specify the query as SPARQL since currently we don't support any
		// more abstract syntax
		final QueryImpl q = new QueryImpl(query, repository, false, connection);
		q.setUseBackupOntology(uow.useBackupOntologyForQueryProcessing());
		return q;
	}

	@Override
	public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass, RepositoryID repository) {
		if (query == null || resultClass == null) {
			throw new NullPointerException("Query or resultClass not specified!");
		}
		if (repository == null) {
			throw new NullPointerException("Repository not specified.");
		}
		final TypedQueryImpl<T> tq = new TypedQueryImpl<T>(query, resultClass, repository, false,
				uow, connection);
		return tq;
	}

}
