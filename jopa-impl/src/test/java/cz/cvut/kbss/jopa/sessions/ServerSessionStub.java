package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;

public class ServerSessionStub extends ServerSession {

    private ConnectionWrapper connection;

    public ServerSessionStub(ConnectionWrapper conn) {
        this.connection = conn;
    }

    protected ConnectionWrapper acquireConnection() {
        return connection;
    }

    @Override
    public Metamodel getMetamodel() {
        // Just exporting API as public so that we can stub it with Mockito
        return null;
    }

    @Override
    protected synchronized void registerEntityWithPersistenceContext(Object entity,
                                                                     UnitOfWorkImpl uow) {
        // do nothing
    }

    @Override
    protected void deregisterEntityFromPersistenceContext(Object entity, UnitOfWork uow) {
        // do nothing
    }

    @Override
    public boolean isTypeManaged(Class<?> cls) {
        return TestEnvironmentUtils.getManagedTypes().contains(cls);
    }
}
