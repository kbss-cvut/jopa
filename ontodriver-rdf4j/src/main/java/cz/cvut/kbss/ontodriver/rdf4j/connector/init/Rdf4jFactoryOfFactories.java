/**
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.rdf4j.connector.init;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectorFactoryImpl;
import cz.cvut.kbss.ontodriver.rdf4j.connector.GraphDBStorageConnector;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.loader.DefaultContextInferenceStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.DefaultStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.GraphDBStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.StatementLoaderFactory;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.query.BooleanQuery;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;

/**
 * Builds factories for the driver.
 */
public class Rdf4jFactoryOfFactories implements FactoryOfFactories {

    /**
     * Property representing internal entity IDs in GraphDB.
     */
    static final String GRAPHDB_INTERNAL_ID_PROPERTY = "http://www.ontotext.com/owlim/entity#id";

    private final DriverConfiguration config;
    private final RepositoryConnectorInitializer connectorInitializer;

    public Rdf4jFactoryOfFactories(DriverConfiguration config) throws Rdf4jDriverException {
        this.config = config;
        this.connectorInitializer = new Rdf4jRepositoryConnectorInitializer(config);
        connectorInitializer.initializeRepository();
    }

    @Override
    public ConnectorFactory createConnectorFactory() throws Rdf4jDriverException {
        final StorageConnector connector;
        if (isRepositoryGraphDB(connectorInitializer.getRepository())) {
            connector = new GraphDBStorageConnector(connectorInitializer);
        } else {
            connector = new StorageConnector(connectorInitializer);
        }
        return new ConnectorFactoryImpl(connector);
    }

    @Override
    public StatementLoaderFactory createStatementLoaderFactory() throws Rdf4jDriverException {
        if (config.is(Rdf4jConfigParam.INFERENCE_IN_DEFAULT_CONTEXT)) {
            return new DefaultContextInferenceStatementLoaderFactory();
        }
        if (isRepositoryGraphDB(connectorInitializer.getRepository())) {
            return new GraphDBStatementLoaderFactory();
        }
        return new DefaultStatementLoaderFactory();
    }

    /**
     * Checks whether the underlying repository is in fact GraphDB.
     * <p>
     * It asks the repository if any subject has an internal GraphDB entity ID (represented by the {@link
     * #GRAPHDB_INTERNAL_ID_PROPERTY}). Such a triple does not normally show in the data, but is accessing in GraphDB.
     * If, for some reason, data stored in a non-GraphDB repository explicitly use these identifiers, this method will
     * return false positive result.
     *
     * @param repository RDF4J repository
     * @return {@code true} if repository is determined to be GraphDB, {@code false} otherwise
     */
    static boolean isRepositoryGraphDB(Repository repository) throws Rdf4jDriverException {
        try {
            try (final RepositoryConnection connection = repository.getConnection()) {
                final ValueFactory vf = connection.getValueFactory();
                // Have to use a SPARQL query, because RDF4J API hasStatement call ended with an error
                // See https://graphdb.ontotext.com/documentation/standard/query-behaviour.html#how-to-access-internal-identifiers-for-entities
                final BooleanQuery query = connection.prepareBooleanQuery("ASK { ?x ?internalId ?y }");
                query.setBinding("internalId", vf.createIRI(GRAPHDB_INTERNAL_ID_PROPERTY));
                return query.evaluate();
            }
        } catch (RuntimeException e) {
            throw new Rdf4jDriverException(e);
        }
    }
}
