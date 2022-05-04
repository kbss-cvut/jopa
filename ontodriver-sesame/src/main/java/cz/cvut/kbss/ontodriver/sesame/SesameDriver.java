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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.ConfigurationParameter;
import cz.cvut.kbss.ontodriver.config.DriverConfigParam;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.config.RuntimeConfiguration;
import cz.cvut.kbss.ontodriver.sesame.config.SesameConfigParam;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactoryImpl;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.sesame.loader.DefaultContextInferenceStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.sesame.loader.DefaultStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.sesame.loader.GraphDBStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.sesame.loader.StatementLoaderFactory;
import org.eclipse.rdf4j.repository.Repository;

import java.util.*;

class SesameDriver implements Closeable, ConnectionListener<SesameConnection> {

    private static final List<ConfigurationParameter> CONFIGS = Arrays
            .asList(DriverConfigParam.AUTO_COMMIT, SesameConfigParam.USE_INFERENCE,
                    SesameConfigParam.USE_VOLATILE_STORAGE, SesameConfigParam.LOAD_ALL_THRESHOLD,
                    SesameConfigParam.RECONNECT_ATTEMPTS, SesameConfigParam.REPOSITORY_CONFIG,
                    SesameConfigParam.INFERENCE_IN_DEFAULT_CONTEXT);

    private final DriverConfiguration configuration;
    private boolean open;
    private final ConnectorFactory connectorFactory;
    private final StatementLoaderFactory statementLoaderFactory;

    private final Set<SesameConnection> openedConnections;

    SesameDriver(OntologyStorageProperties storageProperties,
                 Map<String, String> properties) throws SesameDriverException {
        assert storageProperties != null;
        assert properties != null;

        this.configuration = new DriverConfiguration(storageProperties);
        configuration.addConfiguration(properties, CONFIGS);
        this.openedConnections = new HashSet<>();
        this.connectorFactory = initConnectorFactory(configuration);
        this.statementLoaderFactory = initStatementLoaderFactory(connectorFactory);
        this.open = true;
    }

    private ConnectorFactory initConnectorFactory(DriverConfiguration configuration) throws SesameDriverException {
        return new ConnectorFactoryImpl(configuration);
    }

    private StatementLoaderFactory initStatementLoaderFactory(
            ConnectorFactory connectorFactory) throws SesameDriverException {
        if (configuration.is(SesameConfigParam.INFERENCE_IN_DEFAULT_CONTEXT)) {
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
            for (SesameConnection c : openedConnections) {
                c.removeListener();
                c.close();
            }
            connectorFactory.close();
        } catch (OntoDriverException e) {
            throw e;
        } catch (Exception e) {
            throw new SesameDriverException(e);
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
        final SesameAdapter adapter = new SesameAdapter(connectorFactory.createStorageConnector(), config);
        final SesameConnection c = new SesameConnection(adapter);
        c.setLists(new SesameLists(adapter, c::ensureOpen, c::commitIfAuto));
        c.setTypes(new SesameTypes(adapter, c::ensureOpen, c::commitIfAuto));
        c.setProperties(new SesameProperties(adapter, c::ensureOpen, c::commitIfAuto));
        openedConnections.add(c);
        c.setListener(this);
        return c;
    }

    @Override
    public void connectionClosed(SesameConnection connection) {
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
    void setRepository(Repository repository) throws SesameDriverException {
        assert open;
        connectorFactory.setRepository(repository);
    }
}
