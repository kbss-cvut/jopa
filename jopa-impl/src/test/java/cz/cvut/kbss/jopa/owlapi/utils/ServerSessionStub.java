package cz.cvut.kbss.jopa.owlapi.utils;

import cz.cvut.kbss.jopa.accessors.TransactionOntologyAccessor;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.sessions.CacheManagerImpl;
import cz.cvut.kbss.jopa.sessions.ServerSession;

public class ServerSessionStub extends ServerSession {

	private final TransactionOntologyAccessor accessor;
	private final CacheManager liveObjectCache;

	public ServerSessionStub(TransactionOntologyAccessor accessor) {
		this.accessor = accessor;
		this.liveObjectCache = new CacheManagerImpl(this);
	}

	@Override
	public TransactionOntologyAccessor getOntologyAccessor() {
		return accessor;
	}

	@Override
	public CacheManager getLiveObjectCache() {
		return liveObjectCache;
	}
}
