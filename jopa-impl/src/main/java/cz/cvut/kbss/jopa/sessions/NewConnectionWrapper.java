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

class NewConnectionWrapper implements ConnectionWrapper {

	private final Connection connection;
	private final ObjectOntologyMapper mapper;

	public NewConnectionWrapper(UnitOfWorkImpl uow, Connection connection) {
		this.connection = connection;
		this.mapper = new ObjectOntologyMapperImpl(uow, connection);
	}

	@Override
	public boolean contains(Object primaryKey, Descriptor descriptor) {
		// TODO
		return false;
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, Descriptor descriptor) {
		final URI pkUri = getPrimaryKeyAsUri(primaryKey);
		return mapper.loadEntity(cls, pkUri, descriptor);
	}

	@Override
	public <T> void merge(T entity, Field field, Descriptor repository) {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void persist(Object primaryKey, T entity, Descriptor descriptor) {
		final URI pkUri = getPrimaryKeyAsUri(primaryKey);
		mapper.persistEntity(pkUri, entity, descriptor);
	}

	@Override
	public <T> void remove(Object primaryKey, Descriptor descriptor) {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, Descriptor descriptor) {
		mapper.loadFieldValue(entity, field, descriptor);
	}

	@Override
	public void commit() {
		try {
			connection.commit();
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	public void rollback() {
		try {
			connection.rollback();
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	public void close() {
		try {
			connection.close();
		} catch (Exception e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	public boolean isConsistent(URI context) {
		try {
			return connection.isConsistent(context);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	public List<URI> getContexts() {
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
		// TODO
		return URI.create(primaryKey.toString());
	}
}
