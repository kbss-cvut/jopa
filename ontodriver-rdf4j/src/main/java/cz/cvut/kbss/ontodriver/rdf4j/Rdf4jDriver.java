/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
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
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectorFactoryImpl;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.loader.DefaultContextInferenceStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.DefaultStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.GraphDBStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.StatementLoaderFactory;
import org.eclipse.rdf4j.repository.Repository;

import java.util.*;

class Rdf4jDriver implements Closeable, ConnectionListener<Rdf4jConnection> {

    private static final List<ConfigurationParameter> CONFIGS = Arrays
            .asList(DriverConfigParam.AUTO_COMMIT, Rdf4jConfigParam.USE_INFERENCE,
                    Rdf4jConfigParam.USE_VOLATILE_STORAGE, Rdf4jConfigParam.LOAD_ALL_THRESHOLD,
                    Rdf4jConfigParam.RECONNECT_ATTEMPTS, Rdf4jConfigParam.REPOSITORY_CONFIG,
                    Rdf4jConfigParam.INFERENCE_IN_DEFAULT_CONTEXT);

    private final DriverConfiguration configuration;
    private boolean open;
    private final ConnectorFactory connectorFactory;
    private final StatementLoaderFactory statementLoaderFactory;

    private final Set<Rdf4jConnection> openedConnections;

    Rdf4jDriver(OntologyStorageProperties storageProperties,
                Map<String, String> properties) throws Rdf4jDriverException {
        assert storageProperties != null;
        assert properties != null;

        this.configuration = new DriverConfiguration(storageProperties);
        configuration.addConfiguration(properties, CONFIGS);
        this.openedConnections = new HashSet<>();
        this.connectorFactory = initConnectorFactory(configuration);
        this.statementLoaderFactory = initStatementLoaderFactory(connectorFactory);
        this.open = true;
    }

    private ConnectorFactory initConnectorFactory(DriverConfiguration configuration) throws Rdf4jDriverException {
        return new ConnectorFactoryImpl(configuration);
    }

    private StatementLoaderFactory initStatementLoaderFactory(
            ConnectorFactory connectorFactory) throws Rdf4jDriverException {
        if (configuration.is(Rdf4jConfigParam.INFERENCE_IN_DEFAULT_CONTEXT)) {
            return new DefaultContextInferenceStatementLoaderFactory();
        }
        final Connector connector = connectorFactory.createStorageConnector();
        if (GraphDBStatementLoaderFactory.isRepositoryGraphDB(connector)) {
            return new GraphDBStatementLoaderFactory();
        }
        return new DefaultStatementLoaderFactory();
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
            connectorFactory.close();
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
        final Rdf4jAdapter adapter = new Rdf4jAdapter(connectorFactory.createStorageConnector(), config);
        final Rdf4jConnection c = new Rdf4jConnection(adapter);
        c.setLists(new Rdf4jLists(adapter, c::ensureOpen, c::commitIfAuto));
        c.setTypes(new Rdf4jTypes(adapter, c::ensureOpen, c::commitIfAuto));
        c.setProperties(new Rdf4jProperties(adapter, c::ensureOpen, c::commitIfAuto));
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
        connectorFactory.setRepository(repository);
    }
}
