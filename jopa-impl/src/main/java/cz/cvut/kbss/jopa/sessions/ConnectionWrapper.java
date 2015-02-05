package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.ontodriver.Statement;

public abstract class ConnectionWrapper {

	protected static final Logger LOG = Logger.getLogger(ConnectionWrapper.class.getName());

	protected UnitOfWorkImpl uow;

	abstract <T> boolean contains(Object primaryKey, Class<T> cls, Descriptor descriptor);

    // TODO Replace the force load parameter with something neater
	abstract <T> T find(Class<T> cls, Object primaryKey, Descriptor descriptor, boolean forceLoad);

	abstract <T> void merge(T entity, Field field, Descriptor repository);

	abstract <T> void persist(Object primaryKey, T entity, Descriptor descriptor);

	abstract <T> void remove(Object primaryKey, Class<T> cls, Descriptor descriptor);

	abstract <T> void loadFieldValue(T entity, Field field, Descriptor descriptor);

	abstract void commit();

	abstract void rollback();

	abstract void close();

	abstract boolean isConsistent(URI context);

	abstract List<URI> getContexts();

	public abstract Statement createStatement();

	void setUnitOfWork(UnitOfWorkImpl uow) {
		this.uow = uow;
	}
}
