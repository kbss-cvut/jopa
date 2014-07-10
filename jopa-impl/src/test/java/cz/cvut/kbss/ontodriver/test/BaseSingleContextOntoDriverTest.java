package cz.cvut.kbss.ontodriver.test;

import java.lang.reflect.Field;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class BaseSingleContextOntoDriverTest {

	protected static final Logger LOG = Logger.getLogger(BaseSingleContextOntoDriverTest.class
			.getName());

	protected static final EntityDescriptor DEFAULT_DESCRIPTOR = new EntityDescriptor();

	protected static PersistenceProviderFacade facade;
	protected static StorageConfig storageConfig;

	protected DataSource ds;
	protected Connection c;

	protected <T> void persist(Object pk, T entity) throws OntoDriverException {
		c.persist(pk, entity, DEFAULT_DESCRIPTOR);
	}

	protected <T> T find(Class<T> cls, Object pk) throws OntoDriverException {
		return c.find(cls, pk, DEFAULT_DESCRIPTOR);
	}

	protected boolean contains(Object pk) throws OntoDriverException {
		return c.contains(pk, null);
	}

	protected void remove(Object pk) throws OntoDriverException {
		c.remove(pk, DEFAULT_DESCRIPTOR);
	}

	protected <T> void merge(T entity, Field field) throws OntoDriverException {
		c.merge(entity, field, DEFAULT_DESCRIPTOR);
	}

	protected <T> void loadFieldValue(T entity, Field field) throws OntoDriverException {
		c.loadFieldValue(entity, field, DEFAULT_DESCRIPTOR);
	}

	protected void acquireConnection(String baseName) throws OntoDriverException {
		this.ds = TestEnv.createDataSource(baseName, storageConfig);
		this.c = ds.getConnection(facade);
	}
}
