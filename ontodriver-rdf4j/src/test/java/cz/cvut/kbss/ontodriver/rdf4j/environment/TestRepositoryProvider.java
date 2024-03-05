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
package cz.cvut.kbss.ontodriver.rdf4j.environment;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfigParam;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.Rdf4jDataSource;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactory;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactoryImpl;
import cz.cvut.kbss.ontodriver.rdf4j.connector.RepoConnection;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;

import java.net.URI;

public class TestRepositoryProvider {

    private ConnectionFactory factory;

    public TestRepositoryProvider() {
    }

    public RepoConnection createConnector(boolean useInference) throws Rdf4jDriverException {
        final DriverConfiguration configuration = new DriverConfiguration(storageProperties());

        configuration.setProperty(Rdf4jConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        configuration.setProperty(Rdf4jConfigParam.USE_INFERENCE, Boolean.toString(useInference));
        configuration.setProperty(DriverConfigParam.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        final StorageConnector connectorInitializer = new StorageConnector(configuration);
        connectorInitializer.initializeRepository();
        this.factory = new ConnectionFactoryImpl(connectorInitializer);
        return factory.createStorageConnection();
    }

    public void close() throws OntoDriverException {
        factory.close();
    }

    public static OntologyStorageProperties storageProperties() {
        return OntologyStorageProperties.physicalUri(URI.create("TestStore"))
                                        .driver(Rdf4jDataSource.class.getCanonicalName()).build();
    }
}
