package cz.cvut.kbss.jopa.accessors;

import java.net.URI;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;

class PersistenceProviderProxy implements PersistenceProviderFacade {

	private final Metamodel metamodel;
	private final ServerSession serverSession;

	public PersistenceProviderProxy(Metamodel metamodel, ServerSession serverSession) {
		if (metamodel == null || serverSession == null) {
			throw new NullPointerException();
		}
		this.metamodel = metamodel;
		this.serverSession = serverSession;
	}

	@Override
	public Metamodel getMetamodel() {
		return metamodel;
	}

	@Override
	public <T> T getEntityFromLiveObjectCache(Class<T> cls, Object primaryKey, URI contextUri) {
		if (cls == null || primaryKey == null) {
			throw new NullPointerException();
		}
		T entity = null;
		CacheManager cache = serverSession.getLiveObjectCache();
		cache.acquireReadLock();
		try {
			entity = cache.get(contextUri, cls, primaryKey);
		} finally {
			cache.releaseReadLock();
		}
		return entity;
	}

}
