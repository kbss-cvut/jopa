package cz.cvut.kbss.ontodriver.impl;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.MetamodelNotSetException;
import cz.cvut.kbss.ontodriver.OntoDriverException;
import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.StorageManager;

public class ConnectionImpl implements Connection {

	private static final Logger LOG = Logger.getLogger(ConnectionImpl.class.getName());

	private final StorageManager storageManager;
	private Metamodel metamodel;

	private Context defaultContext;
	private Map<URI, Context> contexts;
	private Map<Object, Context> entityToContext;

	private boolean open;
	private boolean hasChanges;
	private boolean autoCommit;

	public ConnectionImpl(StorageManager storageManager) {
		super();
		if (storageManager == null) {
			throw new NullPointerException();
		}
		this.contexts = new HashMap<URI, Context>();
		// This has to be based on identities
		this.entityToContext = new IdentityHashMap<Object, Context>();
		this.storageManager = storageManager;
		this.open = true;
		this.autoCommit = true;
	}

	public ConnectionImpl(StorageManager storageManager, Metamodel metamodel) {
		this(storageManager);
		this.metamodel = metamodel;
	}

	public void close() throws OntoDriverException {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Closing the connection.");
		}
		if (!open) {
			return;
		}
		storageManager.close();
		this.open = false;
	}

	public void commit() throws OntoDriverException, MetamodelNotSetException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Committing changes.");
		}
		ensureState(true);
		if (!hasChanges) {
			return;
		}
		storageManager.commit();
		afterTransactionFinished();
	}

	public Statement createStatement() throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	public <T> T find(Class<T> cls, Object primaryKey) throws OntoDriverException,
			MetamodelNotSetException {
		ensureState(true);
		if (cls == null || primaryKey == null) {
			LOG.severe("Null argument passed: cls = " + cls + ", primaryKey = " + primaryKey);
			throw new NullPointerException();
		}
		T result = storageManager.find(cls, primaryKey, defaultContext,
				Collections.<String, Context> emptyMap());
		if (result != null) {
			registerInternal(result, defaultContext);
			return result;
		}
		for (Context ctx : storageManager.getAvailableContexts()) {
			// We can use identity here
			if (ctx == defaultContext) {
				continue;
			}
			result = storageManager.find(cls, primaryKey, ctx,
					Collections.<String, Context> emptyMap());
			if (result != null) {
				registerInternal(result, ctx);
				return result;
			}
		}
		return null;
	}

	public <T> T find(Class<T> cls, Object primaryKey, URI context) throws OntoDriverException,
			MetamodelNotSetException {
		ensureState(true);
		if (cls == null || primaryKey == null || context == null) {
			LOG.severe("Null argument passed: cls = " + cls + ", primaryKey = " + primaryKey
					+ ", context = " + context);
			throw new NullPointerException();
		}
		final Context ctx = contexts.get(context);
		if (ctx == null) {
			throw new OntoDriverException("Context with URI " + context.toString()
					+ " not found within this connection.");
		}
		return findInternal(cls, primaryKey, ctx, Collections.<String, Context> emptyMap());
	}

	public <T> T find(Class<T> cls, Object primaryKey, URI entityContext,
			Map<String, URI> attributeContexts) throws OntoDriverException,
			MetamodelNotSetException {
		ensureState(true);
		if (cls == null || primaryKey == null || entityContext == null || attributeContexts == null) {
			LOG.severe("Null argument passed: cls = " + cls + ", primaryKey = " + primaryKey
					+ ", context = " + entityContext + ", attributeContexts = " + attributeContexts);
			throw new NullPointerException();
		}
		final Context ctx = contexts.get(entityContext);
		if (ctx == null) {
			throw new OntoDriverException("Context with URI " + entityContext.toString()
					+ " not found within this connection.");
		}
		Map<String, Context> attContexts = resolveAttributeContexts(attributeContexts);
		return findInternal(cls, primaryKey, ctx, attContexts);
	}

	private <T> T findInternal(Class<T> cls, Object primaryKey, Context entityContext,
			Map<String, Context> attContexts) throws OntoDriverException {
		final T result = storageManager.find(cls, primaryKey, entityContext, attContexts);
		if (result != null) {
			registerInternal(result, entityContext);
		}
		return result;
	}

	public boolean getAutoCommit() throws OntoDriverException {
		ensureState(false);
		return autoCommit;
	}

	public Context getContext(URI contextUri) throws OntoDriverException {
		ensureState(false);
		if (contextUri == null) {
			throw new NullPointerException();
		}
		return contexts.get(contextUri);
	}

	public List<Context> getContexts() throws OntoDriverException {
		ensureState(false);
		return storageManager.getAvailableContexts();
	}

	public Context getSaveContextFor(Object entity) throws OntoDriverException {
		ensureState(false);
		if (entity == null) {
			throw new NullPointerException();
		}
		Context ctx = entityToContext.get(entity);
		if (ctx == null) {
			ctx = defaultContext;
		}
		return ctx;
	}

	public boolean isOpen() {
		return open;
	}

	public <T> void merge(Object primaryKey, T entity) throws OntoDriverException,
			MetamodelNotSetException {
		ensureState(true);
		if (primaryKey == null || entity == null) {
			LOG.severe("Null argument passed: primaryKey = " + primaryKey + ", primaryKey = "
					+ primaryKey);
			throw new NullPointerException();
		}
		final Context ctx = entityToContext.get(entity);
		if (ctx == null) {
			throw new OntoDriverException("The entity " + entity
					+ " is not persistent within this connection.");
		}
		storageManager.merge(primaryKey, entity, ctx, Collections.<String, Context> emptyMap());
		this.hasChanges = true;
		if (autoCommit) {
			commit();
		}
	}

	public <T> void persist(Object primaryKey, T entity) throws OntoDriverException,
			MetamodelNotSetException {
		ensureState(true);
		if (entity == null) {
			LOG.severe("Null argument passed: entity = " + entity);
			throw new NullPointerException();
		}
		Context ctx = null;
		ctx = entityToContext.get(entity);
		if (ctx != null) {
			persistInternal(primaryKey, entity, ctx, Collections.<String, Context> emptyMap());
			return;
		}
		ctx = defaultContext;
		persistInternal(primaryKey, entity, ctx, Collections.<String, Context> emptyMap());
	}

	public <T> void persist(Object primaryKey, T entity, URI context) throws OntoDriverException,
			MetamodelNotSetException {
		ensureState(true);
		if (entity == null || context == null) {
			LOG.severe("Null argument passed: entity = " + entity + ", context = " + context);
			throw new NullPointerException();
		}
		final Context ctx = contexts.get(context);
		if (ctx == null) {
			throw new OntoDriverException("Context with URI " + context.toString()
					+ " not found within this connection.");
		}
		persistInternal(primaryKey, entity, ctx, Collections.<String, Context> emptyMap());
	}

	public <T> void persist(Object primaryKey, T entity, URI context,
			Map<String, URI> attributeContexts) throws OntoDriverException,
			MetamodelNotSetException {
		ensureState(true);
		if (entity == null || context == null || attributeContexts == null) {
			LOG.severe("Null argument passed: entity = " + entity + ", entityContext = " + context
					+ ", attributeContexts = " + attributeContexts);
			throw new NullPointerException();
		}
		final Context ctx = contexts.get(context);
		if (ctx == null) {
			throw new OntoDriverException("Context with URI " + context.toString()
					+ " not found within this connection.");
		}
		final Map<String, Context> attrContexts = resolveAttributeContexts(attributeContexts);
		persistInternal(primaryKey, entity, ctx, attrContexts);
	}

	private <T> void persistInternal(Object primaryKey, T entity, Context context,
			Map<String, Context> attributeContexts) throws OntoDriverException {
		assert entity != null;
		assert context != null;
		assert attributeContexts != null;
		storageManager.persist(primaryKey, entity, context, attributeContexts);
		registerInternal(entity, context);
		if (autoCommit) {
			commit();
		}
	}

	public PreparedStatement prepareStatement(String sparql) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	public <T> void registerWithContext(T entity, URI context) throws OntoDriverException {
		ensureState(false);
		if (entity == null || context == null) {
			throw new NullPointerException();
		}
		final Context ctx = contexts.get(context);
		if (ctx == null) {
			throw new OntoDriverException("Context with URI " + context.toString()
					+ " not found within this connection.");
		}
		registerInternal(entity, ctx);
	}

	public <T> void remove(Object primaryKey, T entity) throws OntoDriverException {
		ensureState(true);
		if (primaryKey == null) {
			LOG.severe("Null argument passed: primaryKey = " + primaryKey);
			throw new NullPointerException();
		}
		Context ctx = entityToContext.get(entity);
		if (ctx == null) {
			throw new OntoDriverException("Context for entity with primary key " + primaryKey
					+ " not found within this connection.");
		}
		removeInternal(primaryKey, entity, ctx);
	}

	public <T> void remove(Object primaryKey, T entity, URI context) throws OntoDriverException {
		ensureState(true);
		if (primaryKey == null || context == null) {
			LOG.severe("Null argument passed: primaryKey = " + primaryKey + ", context = "
					+ context);
			throw new NullPointerException();
		}
		Context ctx = contexts.get(context);
		if (ctx == null) {
			throw new OntoDriverException("Context with URI " + context.toString()
					+ " not found within this connection.");
		}
		if (ctx != entityToContext.get(entity)) {
			throw new OntoDriverException(
					"The context of the entity and the context specified by URI are not the same!");
		}
		removeInternal(primaryKey, entity, ctx);
	}

	private void removeInternal(Object primaryKey, Object entity, Context ctx)
			throws OntoDriverException {
		storageManager.remove(primaryKey, ctx);
		entityToContext.remove(entity);
		if (autoCommit) {
			commit();
		}
	}

	public void rollback() throws OntoDriverException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Rolling back changes.");
		}
		ensureState(false);
		if (!hasChanges) {
			return;
		}
		storageManager.rollback();
		afterTransactionFinished();
	}

	public void setAutoCommit(boolean autoCommit) throws OntoDriverException {
		ensureState(false);
		this.autoCommit = autoCommit;
	}

	public void setConnectionContext(URI context) throws OntoDriverException {
		ensureState(false);
		if (context == null) {
			throw new NullPointerException();
		}
		Context ctx = contexts.get(context);
		if (ctx == null) {
			throw new OntoDriverException("Context with URI " + context.toString()
					+ " not found within this connection.");
		}
		this.defaultContext = ctx;
	}

	public void setMetamodel(Metamodel metamodel) throws OntoDriverException {
		ensureState(false);
		if (metamodel == null) {
			throw new NullPointerException();
		}
		this.metamodel = metamodel;
	}

	public void setSaveContextFor(Object entity, URI context) throws OntoDriverException {
		ensureState(false);
		if (entity == null || context == null) {
			throw new NullPointerException();
		}
		final Context ctx = contexts.get(context);
		if (ctx == null) {
			throw new OntoDriverException("Context with URI " + context.toString()
					+ " not found within this connection.");
		}
		registerInternal(entity, ctx);
	}

	/**
	 * Does cleanup after transaction has finished (either with {@code commit}
	 * or {@code rollback});
	 */
	private void afterTransactionFinished() {
		entityToContext.clear();
		this.hasChanges = false;

	}

	/**
	 * Ensures correct state of this {@code Connection}. </p>
	 * 
	 * This means checking if it is open and, if enabled, whether the metamodel
	 * is set.
	 * 
	 * @param checkMetamodel
	 *            True if the metamodel should be checked
	 * @throws OntoDriverException
	 * @throws MetamodelNotSetException
	 */
	private void ensureState(boolean checkMetamodel) throws OntoDriverException,
			MetamodelNotSetException {
		if (!open) {
			throw new OntoDriverException("The connection is closed.");
		}
		if (checkMetamodel && metamodel == null) {
			throw new MetamodelNotSetException("Metamodel is not set for this Connection.");
		}
	}

	/**
	 * Registers the specified {@code entity} with its context within this
	 * {@code Connection}.
	 * 
	 * @param entity
	 *            The entity
	 * @param ctx
	 *            Context
	 */
	private void registerInternal(Object entity, Context ctx) {
		// Possible to add some more code if necessary
		assert entity != null;
		assert ctx != null;
		entityToContext.put(entity, ctx);
	}

	/**
	 * Resolves attribute contexts based on the map of attribute names and URIs
	 * of contexts.
	 * 
	 * @param ctxs
	 *            Map of attribute name -> context URI
	 * @return Map of attribute name -> Context
	 * @throws OntoDriverException
	 *             If any of the contexts is not valid
	 */
	private Map<String, Context> resolveAttributeContexts(Map<String, URI> ctxs)
			throws OntoDriverException {
		assert ctxs != null;
		final Map<String, Context> result = new HashMap<String, Context>(ctxs.size());
		for (Entry<String, URI> e : ctxs.entrySet()) {
			final Context ctx = contexts.get(e.getValue());
			if (ctx == null) {
				throw new OntoDriverException("Context with URI " + e.getValue()
						+ " not found within this connection.");
			}
		}
		return result;
	}
}
