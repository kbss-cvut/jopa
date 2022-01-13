/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class SesameDriverTest {

    private static OntologyStorageProperties storageProperties;

    @Mock
    private ConnectorFactory connectorFactoryMock;

    @Mock
    private Connector connectorMock;

    private SesameDriver driver;

    @BeforeAll
    public static void setUpBeforeClass() {
        storageProperties = OntologyStorageProperties.physicalUri(URI.create("http://krizik.felk.cvut.cz/repo"))
                .driver(SesameDataSource.class.getCanonicalName())
                .build();
    }

    @BeforeEach
    public void setUp() throws Exception {
        this.driver = new SesameDriver(storageProperties, Collections.emptyMap());
        final Field factoryField = SesameDriver.class.getDeclaredField("connectorFactory");
        factoryField.setAccessible(true);
        factoryField.set(driver, connectorFactoryMock);
    }

    @Test
    public void testClose() throws Exception {
        assertTrue(driver.isOpen());
        driver.close();
        assertFalse(driver.isOpen());
        verify(connectorFactoryMock).close();
    }

    @Test
    public void acquiresConnection() throws Exception {
        when(connectorFactoryMock.createStorageConnector(new DriverConfiguration(storageProperties)))
                .thenReturn(connectorMock);
        final Connection res = driver.acquireConnection();
        assertNotNull(res);
        assertNotNull(res.lists());
        verify(connectorFactoryMock).createStorageConnector(new DriverConfiguration(storageProperties));
        verify(connectorFactoryMock).createStorageConnector(new DriverConfiguration(storageProperties));
    }

    @Test
    public void removesClosedConnectionFromActiveConnections() throws Exception {
        when(connectorFactoryMock.createStorageConnector(new DriverConfiguration(storageProperties)))
                .thenReturn(connectorMock);
        final Connection conn = driver.acquireConnection();
        assertNotNull(conn);
        final Field connectionsField = SesameDriver.class.getDeclaredField("openedConnections");
        connectionsField.setAccessible(true);
        final Set<Connection> openedConnections = (Set<Connection>) connectionsField.get(driver);
        assertTrue(openedConnections.contains(conn));
        conn.close();
        assertFalse(openedConnections.contains(conn));
    }
}
