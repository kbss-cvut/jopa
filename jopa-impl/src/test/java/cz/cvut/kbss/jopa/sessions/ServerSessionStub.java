/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.model.AbstractEntityManager;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.sessions.cache.DisabledCacheManager;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;

import static org.mockito.Mockito.spy;

public class ServerSessionStub extends ServerSession {

    private final ConnectionWrapper connection;

    private final CacheManager disabledCache = spy(new DisabledCacheManager());

    public ServerSessionStub(MetamodelImpl metamodel, ConnectionWrapper conn) {
        super(metamodel);
        this.connection = conn;
    }

    protected ConnectionWrapper acquireConnection() {
        return connection;
    }

    @Override
    public boolean isEntityType(Class<?> cls) {
        return TestEnvironmentUtils.getManagedTypes().contains(cls);
    }

    @Override
    public void transactionStarted(EntityTransaction t, AbstractEntityManager em) {
        // Do nothing
    }

    @Override
    public CacheManager getLiveObjectCache() {
        return disabledCache;
    }
}
