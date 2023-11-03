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

import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This is the implementation of the basic Session operations. Other more
 * specific methods are to be implemented in descendants.
 */
public abstract class AbstractSession implements Session, MetamodelProvider, ConfigurationHolder {

    protected static final Logger LOG = LoggerFactory.getLogger(AbstractSession.class);

    protected Configuration configuration;

    protected AbstractSession(Configuration configuration) {
        this.configuration = configuration;
    }

    @Override
    public UnitOfWork acquireUnitOfWork() {
        // TODO UoW acquisition needs to be provided with configuration from EntityManager, because it may override the server session config
        UnitOfWork uow = new UnitOfWorkImpl(this);
        LOG.trace("UnitOfWork acquired.");
        return uow;
    }

    @Override
    public abstract MetamodelImpl getMetamodel();

    /**
     * This method just releases the live object cache. Subclasses are free to
     * make additional cleanup.
     */
    @Override
    public void release() {
        getLiveObjectCache().evictAll();
    }

    @Override
    public Configuration getConfiguration() {
        return configuration;
    }

    /**
     * Get the current live object cache.
     * <p>
     * This manager represents the second level cache.
     *
     * @return Second level cache
     */
    public abstract CacheManager getLiveObjectCache();

    /**
     * Acquires connection to the underlying ontology storage.
     *
     * @return Connection
     */
    protected abstract ConnectionWrapper acquireConnection();

    /**
     * Gets an object managing named queries in this persistence unit.
     *
     * @return {@link NamedQueryManager}
     */
    public abstract NamedQueryManager getNamedQueryManager();

    /**
     * Gets the manager of SPARQL result set mapping instances.
     * @return {@link ResultSetMappingManager}
     */
    public abstract ResultSetMappingManager getResultSetMappingManager();
}
