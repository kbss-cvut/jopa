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
package cz.cvut.kbss.ontodriver.sesame.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.sesame.config.SesameConfigParam;
import cz.cvut.kbss.ontodriver.sesame.environment.TestUtils;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.sail.SailRepository;
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class ConnectorFactoryImplTest {

    private ConnectorFactory factory;

    @AfterEach
    public void tearDown() throws Exception {
        if (factory != null && factory.isOpen()) {
            factory.close();
        }
    }

    @Test
    public void setRepositoryThrowsIllegalStateWhenCalledOnClosedFactory() throws Exception {
        final DriverConfiguration config = TestUtils.createDriverConfig("urn:test");
        config.setProperty(SesameConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        this.factory = new ConnectorFactoryImpl(config);
        factory.close();
        final Repository repo = new SailRepository(new MemoryStore());
        try {
            assertThrows(IllegalStateException.class, () -> factory.setRepository(repo));
        } finally {
            repo.shutDown();
        }
    }

    @Test
    public void constructorInitializesRepositoryConnection() throws Exception {
        final DriverConfiguration config = TestUtils.createDriverConfig("urn:test");
        config.setProperty(SesameConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        this.factory = new ConnectorFactoryImpl(config);
        final Connector connector = factory.createStorageConnector();
        assertNotNull(connector);
        assertTrue(connector.unwrap(Repository.class).isInitialized());
    }
}
