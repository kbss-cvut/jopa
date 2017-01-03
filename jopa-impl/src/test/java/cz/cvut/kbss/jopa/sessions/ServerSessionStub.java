/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
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
    public MetamodelImpl getMetamodel() {
        // Just exporting API as public so that we can stub it with Mockito
        return null;
    }

    @Override
    protected synchronized void registerEntityWithPersistenceContext(Object entity, UnitOfWorkImpl uow) {
        // do nothing
    }

    @Override
    protected void deregisterEntityFromPersistenceContext(Object entity, UnitOfWork uow) {
        // do nothing
    }

    @Override
    public void releasePersistenceContext(UnitOfWork uow) {
        // Do nothing
    }

    @Override
    public boolean isTypeManaged(Class<?> cls) {
        return TestEnvironmentUtils.getManagedTypes().contains(cls);
    }

    @Override
    public synchronized UnitOfWorkImpl getPersistenceContext(Object entity) {
        return null;
    }
}
