/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame.environment;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.ConfigParam;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.SesameDataSource;
import cz.cvut.kbss.ontodriver.sesame.config.SesameConfigParam;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

import java.net.URI;

public class TestRepositoryProvider {

    private final ConnectorFactory factory = ConnectorFactory.getInstance();

    public Connector createConnector(boolean useInference) throws SesameDriverException {
        OntologyStorageProperties storageProperties = OntologyStorageProperties
                .physicalUri(URI.create("TestStore"))
                .driver(SesameDataSource.class.getCanonicalName())
                .build();
        final Configuration configuration = new Configuration(storageProperties);

        configuration.setProperty(SesameConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        configuration.setProperty(SesameConfigParam.USE_INFERENCE, Boolean.toString(useInference));
        configuration.setProperty(ConfigParam.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        configuration.setProperty(ConfigParam.ONTOLOGY_LANGUAGE, "en");
        return factory.createStorageConnector(configuration);
    }

    public void close() throws OntoDriverException {
        factory.close();
    }
}
