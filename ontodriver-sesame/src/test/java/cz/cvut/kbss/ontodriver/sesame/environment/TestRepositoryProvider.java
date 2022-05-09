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
package cz.cvut.kbss.ontodriver.sesame.environment;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfigParam;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.SesameDataSource;
import cz.cvut.kbss.ontodriver.sesame.config.SesameConfigParam;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactoryImpl;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

import java.net.URI;

public class TestRepositoryProvider {

    private ConnectorFactory factory;

    public TestRepositoryProvider() {
    }

    public Connector createConnector(boolean useInference) throws SesameDriverException {
        final DriverConfiguration configuration = new DriverConfiguration(storageProperties());

        configuration.setProperty(SesameConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        configuration.setProperty(SesameConfigParam.USE_INFERENCE, Boolean.toString(useInference));
        configuration.setProperty(DriverConfigParam.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        this.factory = new ConnectorFactoryImpl(configuration);
        return factory.createStorageConnector();
    }

    public void close() throws OntoDriverException {
        factory.close();
    }

    public static OntologyStorageProperties storageProperties() {
        return OntologyStorageProperties.physicalUri(URI.create("TestStore"))
                .driver(SesameDataSource.class.getCanonicalName()).build();
    }
}
