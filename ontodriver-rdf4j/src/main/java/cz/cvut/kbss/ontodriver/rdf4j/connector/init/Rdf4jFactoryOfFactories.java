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
package cz.cvut.kbss.ontodriver.rdf4j.connector.init;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactory;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactoryConfig;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactoryImpl;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.loader.DefaultContextInferenceStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.DefaultStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.GraphDBStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.StatementLoaderFactory;
import org.eclipse.rdf4j.common.transaction.IsolationLevels;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.query.BooleanQuery;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Optional;
import java.util.stream.Stream;

/**
 * Builds factories for the driver.
 */
public class Rdf4jFactoryOfFactories implements FactoryOfFactories {

    private static final Logger LOG = LoggerFactory.getLogger(Rdf4jFactoryOfFactories.class);

    /**
     * Property representing internal entity IDs in GraphDB.
     */
    static final String GRAPHDB_INTERNAL_ID_PROPERTY = "http://www.ontotext.com/owlim/entity#id";

    private final DriverConfiguration config;
    private final StorageConnector connector;

    private final boolean isGraphDB;

    public Rdf4jFactoryOfFactories(DriverConfiguration config) throws Rdf4jDriverException {
        this.config = config;
        this.connector = new StorageConnector(config);
        connector.initializeRepository();
        this.isGraphDB = isRepositoryGraphDB();
    }

    @Override
    public ConnectionFactory createConnectorFactory() throws Rdf4jDriverException {
        return new ConnectionFactoryImpl(connector, resolveFactoryConfig());
    }

    @Override
    public StatementLoaderFactory createStatementLoaderFactory() {
        if (config.is(Rdf4jConfigParam.INFERENCE_IN_DEFAULT_CONTEXT)) {
            return new DefaultContextInferenceStatementLoaderFactory();
        }
        if (isGraphDB) {
            return new GraphDBStatementLoaderFactory();
        }
        return new DefaultStatementLoaderFactory();
    }

    ConnectionFactoryConfig resolveFactoryConfig() throws Rdf4jDriverException {
        final String isolationLevelConfig = config.getProperty(Rdf4jConfigParam.TRANSACTION_ISOLATION_LEVEL);
        if (isolationLevelConfig != null) {
            final Optional<IsolationLevels> optionalLevel = Stream.of(IsolationLevels.values())
                                                                  .filter(level -> level.toString()
                                                                                        .equals(isolationLevelConfig))
                                                                  .findAny();
            if (optionalLevel.isEmpty()) {
                throw new Rdf4jDriverException("Unsupported transaction isolation level value '" + isolationLevelConfig + "'.");
            }
            LOG.debug("Configured to use RDF4J transaction isolation level '{}'.", optionalLevel.get());
            return new ConnectionFactoryConfig(isGraphDB, optionalLevel.get());
        }
        return new ConnectionFactoryConfig(isGraphDB, null);
    }

    /**
     * Checks whether the underlying repository is in fact GraphDB.
     * <p>
     * It asks the repository if any subject has an internal GraphDB entity ID (represented by the
     * {@link #GRAPHDB_INTERNAL_ID_PROPERTY}). Such a triple does not normally show in the data, but is accessing in
     * GraphDB. If, for some reason, data stored in a non-GraphDB repository explicitly use these identifiers, this
     * method will return false positive result.
     *
     * @return {@code true} if repository is determined to be GraphDB, {@code false} otherwise
     */
    private boolean isRepositoryGraphDB() throws Rdf4jDriverException {
        try {
            try (final RepositoryConnection connection = connector.acquireConnection()) {
                final ValueFactory vf = connection.getValueFactory();
                // Have to use a SPARQL query, because RDF4J API hasStatement call ended with an error
                // See https://graphdb.ontotext.com/documentation/standard/query-behaviour.html#how-to-access-internal-identifiers-for-entities
                final BooleanQuery query = connection.prepareBooleanQuery("ASK { ?x ?internalId ?y }");
                query.setBinding("internalId", vf.createIRI(GRAPHDB_INTERNAL_ID_PROPERTY));
                final boolean result = query.evaluate();
                if (result) {
                    LOG.debug("Underlying repository is GraphDB.");
                }
                return result;
            }
        } catch (RuntimeException e) {
            throw new Rdf4jDriverException(e);
        }
    }
}
