package cz.cvut.kbss.jopa.sessions;

import java.util.Collections;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Level;

import cz.cvut.kbss.jopa.accessors.TransactionOntologyAccessor;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

/**
 * ClientSession are bound to a single client and they provide the access to the
 * underlying ontology.
 * 
 * @author kidney
 * 
 */
public class ClientSession extends AbstractSession {

	private final ServerSession parent;
	private TransactionOntologyAccessor accessor;

	/**
	 * Default constructor. Should not be used.
	 */
	public ClientSession() {
		super();
		parent = null;
	}

	public ClientSession(ServerSession parent) {
		super();
		this.parent = parent;
	}

	@Override
	public UnitOfWork acquireUnitOfWork() {
		UnitOfWork uow = new UnitOfWorkImpl(this);
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("UnitOfWork acquired.");
		}
		return uow;
	}

	public ServerSession getParent() {
		return parent;
	}

	@Override
	public void release() {
		if (accessor.isOpen()) {
			accessor.close();
		}
	}

	@Override
	public void releaseObjectCache() {
		this.parent.releaseObjectCache();
	}

	public CacheManager getLiveObjectCache() {
		return this.parent.getLiveObjectCache();
	}

	public void removeObjectFromCache(Object object) {
		if (object == null) {
			return;
		}
		getLiveObjectCache().removeObjectFromCache(object);
	}

	@Override
	public TransactionOntologyAccessor getOntologyAccessor() {
		// If the accessor is not set or is closed, acquire a new one
		if (accessor == null || !accessor.isOpen()) {
			this.accessor = parent.getOntologyAccessor();
		}
		return accessor;
	}

	public Vector<?> executeQuery(String sparqlQuery) {
		return this.parent.executeQuery(sparqlQuery);
	}

	public Vector<?> readAllObjects(Class<?> domainClass) {
		return this.parent.readAllObjects(domainClass);
	}

	public Object readObject(Class<?> domainClass) {
		return this.parent.readObject(domainClass);
	}

	public <T> T readObject(Class<T> cls, Object primaryKey) {
		return this.parent.readObject(cls, primaryKey);
	}

	public Set<Class<?>> getManagedTypes() {
		if (this.parent == null) {
			return Collections.emptySet();
		}
		return this.parent.getManagedTypes();
	}

	@Override
	Metamodel getMetamodel() {
		return parent.getMetamodel();
	}
}
