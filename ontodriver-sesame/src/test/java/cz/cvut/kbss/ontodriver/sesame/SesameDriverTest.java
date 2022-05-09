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

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.sesame.config.SesameOntoDriverProperties;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

public class SesameDriverTest {

    private static OntologyStorageProperties storageProperties;

    private SesameDriver driver;

    @BeforeAll
    public static void setUpBeforeClass() {
        storageProperties = OntologyStorageProperties.physicalUri(URI.create("mem:test"))
                .driver(SesameDataSource.class.getCanonicalName())
                .build();
    }

    @BeforeEach
    public void setUp() throws Exception {
        final Map<String, String> properties = new HashMap<>();
        properties.put(SesameOntoDriverProperties.SESAME_USE_VOLATILE_STORAGE, Boolean.toString(true));
        this.driver = new SesameDriver(storageProperties, properties);
    }

    @Test
    public void testClose() throws Exception {
        final Field connectorFactoryField = SesameDriver.class.getDeclaredField("connectorFactory");
        connectorFactoryField.setAccessible(true);
        final ConnectorFactory connectorFactory = (ConnectorFactory) connectorFactoryField.get(driver);
        assertTrue(driver.isOpen());
        driver.close();
        assertFalse(driver.isOpen());
        assertFalse(connectorFactory.isOpen());
    }

    @Test
    public void acquiresConnection() {
        final Connection res = driver.acquireConnection();
        assertNotNull(res);
        assertNotNull(res.lists());
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
