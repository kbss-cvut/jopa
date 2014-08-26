package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.MetamodelNotSetException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

class LegacyConnectionWrapper implements ConnectionWrapper {

	private static final Logger LOG = Logger.getLogger(LegacyConnectionWrapper.class.getName());

	private final UnitOfWorkImpl uow;
	private Connection connection;

	LegacyConnectionWrapper(UnitOfWorkImpl uow, Connection connection) {
		this.uow = uow;
		this.connection = connection;
	}

	@Override
	public boolean contains(Object primaryKey, Descriptor descriptor) {
		assert primaryKey != null;
		try {
			return connection.contains(primaryKey, descriptor.getContext());
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, Descriptor descriptor) {
		assert cls != null;
		assert primaryKey != null;
		try {
			final T result = connection.find(cls, primaryKey, descriptor);
			if (result != null) {
				// Put into cache here, when we are sure that the entity is in
				// the ontology
				uow.putObjectIntoCache(primaryKey, result, descriptor.getContext());
			}
			return result;
		} catch (MetamodelNotSetException | OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	public <T> void merge(T entity, Field field, Descriptor repository) {
		assert entity != null;
		assert repository != null;
		try {
			connection.merge(entity, field, repository);
		} catch (MetamodelNotSetException e) {
			throw new OWLPersistenceException(e);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	public <T> void persist(Object primaryKey, T entity, Descriptor descriptor) {
		assert entity != null;
		assert descriptor != null;
		try {
			connection.persist(primaryKey, entity, descriptor);
		} catch (MetamodelNotSetException e) {
			throw new OWLPersistenceException(e);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	public <T> void remove(Object primaryKey, Descriptor descriptor) {
		assert primaryKey != null;
		assert descriptor != null;
		try {
			connection.remove(primaryKey, descriptor);
		} catch (MetamodelNotSetException e) {
			throw new OWLPersistenceException(e);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, Descriptor descriptor) {
		try {
			connection.loadFieldValue(entity, field, descriptor);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	public void commit() {
		try {
			connection.commit();
		} catch (Exception e) {
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
		} catch (OntoDriverException e) {
			LOG.log(Level.SEVERE, "Exception caugth when closing connection.", e);
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

}
