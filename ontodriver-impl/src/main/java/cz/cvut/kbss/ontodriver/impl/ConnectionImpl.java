package cz.cvut.kbss.ontodriver.impl;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.MetamodelNotSetException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class ConnectionImpl implements Connection {

	private static final Logger LOG = Logger.getLogger(ConnectionImpl.class.getName());

	private final StorageModule storageModule;

	private boolean open;
	private boolean hasChanges;
	private boolean autoCommit;

	public ConnectionImpl(StorageModule storageModule) throws OntoDriverException {
		this.storageModule = Objects.requireNonNull(storageModule,
				ErrorUtils.constructNPXMessage("storageModule"));

		this.open = true;
		this.hasChanges = false;
		this.autoCommit = true;
	}

	@Override
	public void close() throws OntoDriverException {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Closing the connection.");
		}
		if (!open) {
			return;
		}
		storageModule.close();
		this.open = false;
	}

	@Override
	public void commit() throws OntoDriverException, MetamodelNotSetException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Committing changes.");
		}
		ensureOpen();
		if (!hasChanges) {
			return;
		}
		storageModule.commit();
		afterTransactionFinished();
	}

	public Statement createStatement() throws OntoDriverException {
		return new JopaStatement(storageModule);
	}

	@Override
	public boolean contains(Object primaryKey, URI context) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(context, ErrorUtils.constructNPXMessage("context"));

		return storageModule.contains(primaryKey, context);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, EntityDescriptor descriptor)
			throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

		final T result = storageModule.find(cls, primaryKey, descriptor);
		return result;
	}

	@Override
	public boolean getAutoCommit() throws OntoDriverException {
		ensureOpen();
		return autoCommit;
	}

	@Override
	public boolean isConsistent(URI context) throws OntoDriverException {
		ensureOpen();

		return storageModule.isConsistent(context);
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, EntityDescriptor descriptor)
			throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(field, ErrorUtils.constructNPXMessage("field"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

		storageModule.loadFieldValue(entity, field, descriptor);
	}

	@Override
	public <T> void merge(T entity, Field mergedField, EntityDescriptor descriptor)
			throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(mergedField, ErrorUtils.constructNPXMessage("mergedField"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

		storageModule.merge(entity, mergedField, descriptor);
		this.hasChanges = true;
		if (autoCommit) {
			commit();
		}
	}

	@Override
	public <T> void persist(Object primaryKey, T entity, EntityDescriptor descriptor)
			throws OntoDriverException {
		ensureOpen();
		// Primary key can be null
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

		storageModule.persist(primaryKey, entity, descriptor);
		this.hasChanges = true;
		if (autoCommit) {
			commit();
		}
	}

	@Override
	public PreparedStatement prepareStatement(String sparql) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> void remove(Object primaryKey, EntityDescriptor descriptor)
			throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

		storageModule.remove(primaryKey, descriptor);
		this.hasChanges = true;
		if (autoCommit) {
			commit();
		}
	}

	@Override
	public void rollback() throws OntoDriverException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Rolling back changes.");
		}
		ensureOpen();
		if (!hasChanges) {
			return;
		}
		storageModule.rollback();
		afterTransactionFinished();
	}

	@Override
	public void setAutoCommit(boolean autoCommit) throws OntoDriverException {
		ensureOpen();
		this.autoCommit = autoCommit;
	}

	/**
	 * Does cleanup after transaction has finished (either with {@code commit}
	 * or {@code rollback});
	 */
	private void afterTransactionFinished() {
		this.hasChanges = false;
	}

	/**
	 * Ensures correct state of this {@code Connection}. </p>
	 * 
	 * 
	 * @throws IllegalStateException
	 *             If the connection is closed
	 */
	private void ensureOpen() {
		if (!open) {
			throw new IllegalStateException("The connection is closed.");
		}
	}

	@Override
	public List<URI> getContexts() throws OntoDriverException {
		ensureOpen();
		return storageModule.getContexts();
	}
}
