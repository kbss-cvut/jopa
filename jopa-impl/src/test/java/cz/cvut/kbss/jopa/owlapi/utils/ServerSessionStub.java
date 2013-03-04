package cz.cvut.kbss.jopa.owlapi.utils;

import java.util.Collections;
import java.util.Set;

import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.sessions.CacheManagerImpl;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.ontodriver.Connection;

public class ServerSessionStub extends ServerSession {

	private final CacheManager liveObjectCache;

	public ServerSessionStub() {
		this.liveObjectCache = new CacheManagerImpl(this,
				Collections.<String, String> emptyMap());
	}

	@Override
	public CacheManager getLiveObjectCache() {
		return liveObjectCache;
	}

	@Override
	public Connection acquireConnection() {
		return null;
	}

	@Override
	public Set<Class<?>> getManagedTypes() {
		return Collections.emptySet();
	}
}
