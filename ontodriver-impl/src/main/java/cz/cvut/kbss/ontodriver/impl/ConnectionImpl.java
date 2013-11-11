package cz.cvut.kbss.ontodriver.impl;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.StorageManager;
import cz.cvut.kbss.ontodriver.exceptions.EntityNotRegisteredException;
import cz.cvut.kbss.ontodriver.exceptions.MetamodelNotSetException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class ConnectionImpl implements Connection {

	private static final Logger LOG = Logger.getLogger(ConnectionImpl.class.getName());

	private final StorageManager storageManager;

	private Context defaultContext;
	private Map<URI, Context> contexts;
	private Map<Object, Context> entityToContext;

	private boolean open;
	private boolean hasChanges;
	private boolean autoCommit;

	public ConnectionImpl(StorageManager storageManager) throws OntoDriverException {
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
		initContexts();
	}

	@Override
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

	@Override
	public void commit() throws OntoDriverException, MetamodelNotSetException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Committing changes.");
		}
		ensureOpen(true);
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

	@Override
	public boolean contains(Object primaryKey) throws OntoDriverException {
		ensureOpen(false);
		if (primaryKey == null) {
			LOG.severe("Null argument passed: primaryKey = " + primaryKey);
			throw new NullPointerException();
		}
		return storageManager.contains(primaryKey, defaultContext);
	}

	@Override
	public boolean contains(Object primaryKey, URI context) throws OntoDriverException {
		ensureOpen(false);
		if (primaryKey == null || context == null) {
			LOG.severe("Null argument passed: primaryKey = " + primaryKey + ", context = "
					+ context);
			throw new NullPointerException();
		}
		final Context ctx = contexts.get(context);
		if (ctx == null) {
			throw new OntoDriverException("Context with URI " + context.toString()
					+ " not found within this connection.");
		}
		return storageManager.contains(primaryKey, ctx);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey) throws OntoDriverException {
		ensureOpen(true);
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

	public <T> T find(Class<T> cls, Object primaryKey, URI context) throws OntoDriverException {
		ensureOpen(true);
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

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, URI entityContext,
			Map<String, URI> attributeContexts) throws OntoDriverException {
		ensureOpen(true);
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

	@Override
	public boolean getAutoCommit() throws OntoDriverException {
		ensureOpen(false);
		return autoCommit;
	}

	@Override
	public Context getContext(URI contextUri) throws OntoDriverException {
		ensureOpen(false);
		if (contextUri == null) {
			throw new NullPointerException();
		}
		return contexts.get(contextUri);
	}

	@Override
	public Context getCurrentContext() throws OntoDriverException {
		return defaultContext;
	}

	@Override
	public List<Context> getContexts() throws OntoDriverException {
		ensureOpen(false);
		return storageManager.getAvailableContexts();
	}

	@Override
	public Context getSaveContextFor(Object entity) throws OntoDriverException {
		ensureOpen(false);
		if (entity == null) {
			throw new NullPointerException();
		}
		Context ctx = entityToContext.get(entity);
		if (ctx == null) {
			ctx = defaultContext;
		}
		return ctx;
	}

	@Override
	public boolean isConsistent(URI context) throws OntoDriverException {
		if (context == null) {
			throw new NullPointerException();
		}
		final Context ctx = contexts.get(context);
		if (ctx == null) {
			throw new OntoDriverException("Unknown context URI " + context);
		}
		return storageManager.isConsistent(ctx);
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field) throws OntoDriverException {
		if (entity == null || field == null) {
			LOG.severe("Null argument passed: entity = " + entity + ", field = " + field);
			throw new NullPointerException();
		}
		final Context ctx = entityToContext.get(entity);
		if (ctx == null) {
			throw new EntityNotRegisteredException("Entity " + entity
					+ " is not registered within this connection.");
		}
		storageManager.loadFieldValue(entity, field, ctx);
	}

	@Override
	public <T> void merge(Object primaryKey, T entity) throws OntoDriverException {
		ensureOpen(true);
		if (primaryKey == null || entity == null) {
			LOG.severe("Null argument passed: primaryKey = " + primaryKey + ", primaryKey = "
					+ primaryKey);
			throw new NullPointerException();
		}
		final Context ctx = entityToContext.get(entity);
		if (ctx == null) {
			throw new EntityNotRegisteredException("Entity " + entity
					+ " is not registered within this connection.");
		}
		storageManager.merge(primaryKey, entity, ctx, Collections.<String, Context> emptyMap());
		this.hasChanges = true;
		if (autoCommit) {
			commit();
		}
	}

	@Override
	public <T> void persist(Object primaryKey, T entity) throws OntoDriverException {
		ensureOpen(true);
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

	@Override
	public <T> void persist(Object primaryKey, T entity, URI context) throws OntoDriverException {
		ensureOpen(true);
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

	@Override
	public <T> void persist(Object primaryKey, T entity, URI context,
			Map<String, URI> attributeContexts) throws OntoDriverException {
		ensureOpen(true);
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
		this.hasChanges = true;
		registerInternal(entity, context);
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
	public <T> void registerWithContext(T entity, URI context) throws OntoDriverException {
		ensureOpen(false);
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

	@Override
	public <T> void remove(Object primaryKey, T entity) throws OntoDriverException {
		ensureOpen(true);
		if (primaryKey == null) {
			LOG.severe("Null argument passed: primaryKey = " + primaryKey);
			throw new NullPointerException();
		}
		Context ctx = entityToContext.get(entity);
		if (ctx == null) {
			throw new EntityNotRegisteredException("Entity  " + entity
					+ " not registered within this connection.");
		}
		removeInternal(primaryKey, entity, ctx);
	}

	@Override
	public <T> void remove(Object primaryKey, T entity, URI context) throws OntoDriverException {
		ensureOpen(true);
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
		this.hasChanges = true;
		entityToContext.remove(entity);
		if (autoCommit) {
			commit();
		}
	}

	@Override
	public void rollback() throws OntoDriverException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Rolling back changes.");
		}
		ensureOpen(false);
		if (!hasChanges) {
			return;
		}
		storageManager.rollback();
		afterTransactionFinished();
	}

	@Override
	public void setAutoCommit(boolean autoCommit) throws OntoDriverException {
		ensureOpen(false);
		this.autoCommit = autoCommit;
	}

	@Override
	public void setConnectionContext(URI context) throws OntoDriverException {
		ensureOpen(false);
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

	@Override
	public void setSaveContextFor(Object entity, URI context) throws OntoDriverException {
		ensureOpen(false);
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

	private void initContexts() throws OntoDriverException {
		final List<Context> ctxs = getContexts();
		for (Context ctx : ctxs) {
			contexts.put(ctx.getUri(), ctx);
		}
		// Set the default context
		this.defaultContext = ctxs.get(0);
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
	private void ensureOpen(boolean checkMetamodel) throws OntoDriverException,
			MetamodelNotSetException {
		if (!open) {
			throw new OntoDriverException("The connection is closed.");
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
