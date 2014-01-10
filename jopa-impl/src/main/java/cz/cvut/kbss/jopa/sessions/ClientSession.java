package cz.cvut.kbss.jopa.sessions;

import java.util.Collections;
import java.util.Set;
import java.util.logging.Level;

import org.semanticweb.owlapi.model.IRI;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.Connection;

/**
 * ClientSession are bound to a single client and they provide the access to the
 * underlying ontology.
 * 
 * @author kidney
 * 
 */
public class ClientSession extends AbstractSession {

	private final ServerSession parent;

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
		final IRI primaryKey = EntityPropertiesUtils.getPrimaryKey(object, getMetamodel());
		if (primaryKey == null) {
			return;
		}
		getLiveObjectCache().evict(object.getClass(), primaryKey);
	}

	@Override
	protected Connection acquireConnection() {
		return parent.acquireConnection();
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

	@Override
	void registerEntityWithContext(Object entity, UnitOfWorkImpl uow) {
		parent.registerEntityWithContext(entity, uow);
	}
}
