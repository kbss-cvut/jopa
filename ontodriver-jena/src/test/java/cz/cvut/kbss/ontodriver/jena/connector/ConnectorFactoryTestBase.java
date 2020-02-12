/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

public abstract class ConnectorFactoryTestBase {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    abstract ConnectorFactory connectorFactory(DriverConfiguration configuration);

    abstract SharedStorageConnector getCentralConnector(ConnectorFactory factory) throws Exception;

    @Test
    public void closeClosesCentralConnector() throws Exception {
        final DriverConfiguration configuration = StorageTestUtil.createConfiguration("test:uri");
        final ConnectorFactory factory = connectorFactory(configuration);
        assertTrue(getCentralConnector(factory).isOpen());
        factory.close();
        assertFalse(factory.isOpen());
        assertFalse(getCentralConnector(factory).isOpen());
    }

    @Test
    public void createConnectorOnClosedFactoryThrowsIllegalStateException() throws Exception {
        thrown.expect(IllegalStateException.class);
        final DriverConfiguration configuration = StorageTestUtil.createConfiguration("test:uri");
        final ConnectorFactory factory = connectorFactory(configuration);
        factory.close();
        assertFalse(factory.isOpen());
        factory.createConnector();
    }

    @Test
    public void createInferredConnectorReturnsCorrectConnector() {
        final DriverConfiguration configuration = StorageTestUtil.createConfiguration("test:uri");
        final ConnectorFactory factory = connectorFactory(configuration);
        final StorageConnector connector = factory.createConnector();
        final InferredStorageConnector result = factory.createInferredConnector(connector);
        assertTrue(result instanceof DummyInferredStorageConnector);
    }

    @Test
    public void reloadStorageReloadsSharedConnectorStorage() throws Exception {
        final DriverConfiguration configuration = StorageTestUtil.createConfiguration("test:uri");
        final ConnectorFactory factory = connectorFactory(configuration);
        final SharedStorageConnector sharedStorageConnector = getCentralConnector(factory);
        sharedStorageConnector.storage = spy(sharedStorageConnector.storage);

        factory.reloadStorage();
        verify(sharedStorageConnector.storage).reload();
    }
}
