package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.lang.reflect.Field;
import java.util.concurrent.atomic.AtomicInteger;

import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.jena.OwlapiBasedJenaConnector;

public class OwlapiBasedCachingJenaModule extends OwlapiStorageModule {

	private OwlapiBasedJenaConnector connector;
	private ModuleInternal moduleInternal;

	private OwlapiConnectorDataHolder data;

	public OwlapiBasedCachingJenaModule(Context context,
			PersistenceProviderFacade persistenceProvider, DriverFactory factory)
			throws OntoDriverException {
		super(context, persistenceProvider, factory);
	}

	@Override
	public void close() throws OntoDriverException {
		factory.releaseStorageConnector(connector);
		this.moduleInternal = null;
		this.open = false;
	}

	@Override
	public void commit() throws OntoDriverException {
		ensureOpen();
		moduleInternal.commitAndRetrieveChanges();
		connector.applyOntologyChanges(data.getOntologyManager(), data.getWorkingOntology());
		connector.saveOntology();
	}

	@Override
	public void rollback() throws OntoDriverException {
		ensureOpen();
		moduleInternal.rollback();
	}

	@Override
	protected void initialize() throws OntoDriverException {
		this.connector = (OwlapiBasedJenaConnector) factory.createStorageConnector(context, false);
		final OwlapiConnectorDataHolder holder = getOntologyData();
		if (!primaryKeyCounters.containsKey(context)) {
			primaryKeyCounters.put(context,
					new AtomicInteger(connector.getClassAssertionAxiomsCount()));
		}
		this.moduleInternal = new ModuleInternalImpl(holder, this);
	}

	@Override
	public boolean contains(Object primaryKey) throws OntoDriverException {
		ensureOpen();
		if (primaryKey == null) {
			throw new NullPointerException("Null passed to contains: primaryKey = " + primaryKey);
		}
		return moduleInternal.containsEntity(primaryKey);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey) throws OntoDriverException {
		ensureOpen();
		if (cls == null || primaryKey == null) {
			throw new NullPointerException("Null passed to find: cls = " + cls + ", primaryKey = "
					+ primaryKey);
		}
		return moduleInternal.findEntity(cls, primaryKey);
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field) throws OntoDriverException {
		ensureOpen();
		if (entity == null || field == null) {
			throw new NullPointerException("Null passed to loadFieldValues: entity = " + entity
					+ ", fieldName = " + field);
		}
		moduleInternal.loadFieldValue(entity, field);
	}

	@Override
	public <T> void merge(Object primaryKey, T entity) throws OntoDriverException {
		ensureOpen();
		if (primaryKey == null || entity == null) {
			throw new NullPointerException("Null passed to merge: primaryKey = " + primaryKey
					+ ", entity = " + entity);
		}
		moduleInternal.mergeEntity(primaryKey, entity);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity) throws OntoDriverException {
		ensureOpen();
		if (entity == null) {
			throw new NullPointerException("Null passed to persist: entity = " + entity);
		}
		moduleInternal.persistEntity(primaryKey, entity);
	}

	@Override
	public void remove(Object primaryKey) throws OntoDriverException {
		ensureOpen();
		if (primaryKey == null) {
			throw new NullPointerException("Null passed to remove: primaryKey = " + primaryKey);
		}
		moduleInternal.removeEntity(primaryKey);
	}

	@Override
	public ResultSet executeStatement(Statement statement) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * Returns cloned ontology structures that can be manipulated without
	 * affecting the original data.
	 * 
	 * @return OwlapiConnectorDataHolder
	 * @throws OntoDriverException
	 *             If an error during cloning occurs
	 */
	public OwlapiConnectorDataHolder cloneOntologyData() throws OntoDriverException {
		return data;
	}

	/**
	 * Returns the original ontology structures directly from the connector.
	 * 
	 * @return OwlapiConnectorDataHolder
	 * @throws OntoDriverException
	 */
	public OwlapiConnectorDataHolder getOntologyData() {
		try {
			this.data = connector.getOntologyDataInOwlapi();
			return data;
		} catch (OntoDriverException e) {
			throw new RuntimeException(e);
		}
	}
}
