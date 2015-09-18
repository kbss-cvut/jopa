package cz.cvut.kbss.jopa.accessors;

import java.net.URI;
import java.util.Objects;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;

class PersistenceProviderProxy implements PersistenceProviderFacade {

    private final Metamodel metamodel;

    private final ServerSession serverSession;

    public PersistenceProviderProxy(Metamodel metamodel, ServerSession serverSession) {
        this.metamodel = Objects.requireNonNull(metamodel,
                ErrorUtils.constructNPXMessage("metamodel"));
        this.serverSession = Objects.requireNonNull(serverSession,
                ErrorUtils.constructNPXMessage("serverSession"));
    }

    @Override
    public Metamodel getMetamodel() {
        return metamodel;
    }

    @Override
    public <T> T getEntityFromLiveObjectCache(Class<T> cls, Object primaryKey, URI context) {
        Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
        Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));

        final CacheManager cache = serverSession.getLiveObjectCache();
        return cache.get(cls, primaryKey, context);
    }

}
