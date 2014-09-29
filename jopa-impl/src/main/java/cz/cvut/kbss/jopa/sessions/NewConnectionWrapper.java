package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.oom.ObjectOntologyMapper;
import cz.cvut.kbss.jopa.oom.ObjectOntologyMapperImpl;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.Connection;

class NewConnectionWrapper extends ConnectionWrapper {

	private final Connection connection;
	private ObjectOntologyMapper mapper;

	public NewConnectionWrapper(Connection connection) {
		this.connection = connection;

	}

	@Override
	void setUnitOfWork(UnitOfWorkImpl uow) {
		this.uow = uow;
		this.mapper = new ObjectOntologyMapperImpl(uow, connection);
	}

	@Override
	boolean contains(Object primaryKey, Descriptor descriptor) {
		// TODO
		return false;
	}

	@Override
	<T> T find(Class<T> cls, Object primaryKey, Descriptor descriptor) {
		final URI pkUri = getPrimaryKeyAsUri(primaryKey);
		return mapper.loadEntity(cls, pkUri, descriptor);
	}

	@Override
	<T> void merge(T entity, Field field, Descriptor repository) {
		// TODO Auto-generated method stub

	}

	@Override
	<T> void persist(Object primaryKey, T entity, Descriptor descriptor) {
		final URI pkUri = getPrimaryKeyAsUri(primaryKey);
		mapper.persistEntity(pkUri, entity, descriptor);
	}

	@Override
	<T> void remove(Object primaryKey, Descriptor descriptor) {
		// TODO Auto-generated method stub

	}

	@Override
	<T> void loadFieldValue(T entity, Field field, Descriptor descriptor) {
		mapper.loadFieldValue(entity, field, descriptor);
	}

	@Override
	void commit() {
		try {
			mapper.checkForUnpersistedChanges();
			connection.commit();
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	void rollback() {
		try {
			connection.rollback();
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	void close() {
		try {
			connection.close();
		} catch (Exception e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	boolean isConsistent(URI context) {
		try {
			return connection.isConsistent(context);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	List<URI> getContexts() {
		try {
			return connection.getContexts();
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	public Statement createStatement() {
		try {
			return connection.createStatement();
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private URI getPrimaryKeyAsUri(Object primaryKey) {
		if (primaryKey == null) {
			return null;
		}
		if (primaryKey instanceof URI) {
			return (URI) primaryKey;
		}
		return URI.create(primaryKey.toString());
	}
}
