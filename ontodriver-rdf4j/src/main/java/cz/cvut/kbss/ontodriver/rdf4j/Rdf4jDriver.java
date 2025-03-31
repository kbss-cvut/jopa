/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.ConfigurationParameter;
import cz.cvut.kbss.ontodriver.config.DriverConfigParam;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.config.RuntimeConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactory;
import cz.cvut.kbss.ontodriver.rdf4j.connector.init.Rdf4jFactoryOfFactories;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.loader.StatementLoaderFactory;
import org.eclipse.rdf4j.repository.Repository;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

class Rdf4jDriver implements Closeable, ConnectionListener<Rdf4jConnection> {

    private static final List<ConfigurationParameter> CONFIGS = List.of(DriverConfigParam.AUTO_COMMIT,
            Rdf4jConfigParam.USE_INFERENCE, Rdf4jConfigParam.USE_VOLATILE_STORAGE, Rdf4jConfigParam.LOAD_ALL_THRESHOLD,
            Rdf4jConfigParam.RECONNECT_ATTEMPTS, Rdf4jConfigParam.REPOSITORY_CONFIG,
            Rdf4jConfigParam.INFERENCE_IN_DEFAULT_CONTEXT, Rdf4jConfigParam.TRANSACTION_ISOLATION_LEVEL,
            Rdf4jConfigParam.MAX_CONNECTION_POOL_SIZE, Rdf4jConfigParam.CONNECTION_REQUEST_TIMEOUT);

    private final DriverConfiguration configuration;
    private boolean open;
    private final ConnectionFactory connectionFactory;
    private final StatementLoaderFactory statementLoaderFactory;

    private final Set<Rdf4jConnection> openedConnections;

    Rdf4jDriver(OntologyStorageProperties storageProperties,
                Map<String, String> properties) throws Rdf4jDriverException {
        assert storageProperties != null;
        assert properties != null;

        this.configuration = new DriverConfiguration(storageProperties);
        configuration.addConfiguration(properties, CONFIGS);
        this.openedConnections = new HashSet<>();
        final Rdf4jFactoryOfFactories factory = new Rdf4jFactoryOfFactories(configuration);
        this.connectionFactory = factory.createConnectorFactory();
        this.statementLoaderFactory = factory.createStatementLoaderFactory();
        this.open = true;
    }

    @Override
    public void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        try {
            for (Rdf4jConnection c : openedConnections) {
                c.removeListener();
                c.close();
            }
            connectionFactory.close();
        } catch (OntoDriverException e) {
            throw e;
        } catch (Exception e) {
            throw new Rdf4jDriverException(e);
        } finally {
            this.open = false;
        }
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    Connection acquireConnection() {
        assert open;
        final RuntimeConfiguration config = new RuntimeConfiguration(configuration);
        config.setStatementLoaderFactory(statementLoaderFactory);
        final Rdf4jAdapter adapter = new Rdf4jAdapter(connectionFactory.createStorageConnection(), config);
        final Rdf4jConnection c = new Rdf4jConnection(adapter);
        c.setLists(new Rdf4jLists(adapter, c::ensureOpen, c::commitIfAuto));
        c.setTypes(new Rdf4jTypes(adapter, c::ensureOpen, c::commitIfAuto));
        c.setProperties(new Rdf4jProperties(adapter, c::ensureOpen, c::commitIfAuto));
        c.setContainers(new Rdf4jContainers(adapter, c::ensureOpen, c::commitIfAuto));
        openedConnections.add(c);
        c.setListener(this);
        return c;
    }

    @Override
    public void connectionClosed(Rdf4jConnection connection) {
        if (connection == null) {
            return;
        }
        openedConnections.remove(connection);
    }

    /**
     * Sets the underlying repository.
     * <p>
     * Note that this functionality is supported only for in-memory stores.
     *
     * @param repository The new repository
     */
    void setRepository(Repository repository) throws Rdf4jDriverException {
        assert open;
        connectionFactory.setRepository(repository);
    }
}
