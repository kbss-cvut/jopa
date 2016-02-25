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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import static org.junit.Assert.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class SesameDriverTest {

    private static OntologyStorageProperties storageProperties;
    private static Map<String, String> properties;

    private ConnectorFactory originalFactory;

    @Mock
    private ConnectorFactory connectorFactoryMock;

    @Mock
    private Connector connectorMock;

    private SesameDriver driver;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        storageProperties = OntologyStorageProperties.physicalUri(URI.create("http://krizik.felk.cvut.cz/repo"))
                                                     .driver(SesameDataSource.class.getCanonicalName())
                                                     .build();
        properties = Collections.emptyMap();
    }

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(connectorFactoryMock.isOpen()).thenReturn(Boolean.TRUE);
        when(connectorFactoryMock.createStorageConnector(storageProperties, properties))
                .thenReturn(connectorMock);
        final Field instanceField = ConnectorFactory.class.getDeclaredField("instance");
        instanceField.setAccessible(true);
        this.originalFactory = (ConnectorFactory) instanceField.get(null);
        instanceField.set(null, connectorFactoryMock);

        this.driver = new SesameDriver(storageProperties, properties);
    }

    @After
    public void tearDown() throws Exception {
        final Field instanceField = ConnectorFactory.class.getDeclaredField("instance");
        instanceField.setAccessible(true);
        instanceField.set(null, originalFactory);
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
        final Connection res = driver.acquireConnection();
        assertNotNull(res);
        assertNotNull(res.lists());
        verify(connectorFactoryMock).createStorageConnector(storageProperties, properties);
        verify(connectorFactoryMock).createStorageConnector(storageProperties, properties);
    }

    @Test
    public void removesClosedConnectionFromActiveConnections() throws Exception {
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
