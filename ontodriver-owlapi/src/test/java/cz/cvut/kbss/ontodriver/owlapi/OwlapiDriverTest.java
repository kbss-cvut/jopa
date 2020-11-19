/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.connector.ConnectorFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class OwlapiDriverTest {

    private static final OntologyStorageProperties STORAGE_PROPERTIES = OntologyStorageProperties.ontologyUri(
            URI.create("http://krizik.felk.cvut.cz/ontologies/jopa")).physicalUri(
            URI.create("http://example.com")).driver(OwlapiDataSource.class.getCanonicalName()).build();


    @Mock
    private ConnectorFactory factoryMock;

    @Mock
    private Connector connectorMock;

    private OwlapiDriver driver;

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final Field instanceField = OwlapiDriver.class.getDeclaredField("connectorFactory");
        instanceField.setAccessible(true);
        this.factoryMock = mock(ConnectorFactory.class);
        when(factoryMock.getConnector(any(DriverConfiguration.class))).thenReturn(connectorMock);
        when(factoryMock.isOpen()).thenReturn(true);
        this.driver = new OwlapiDriver(STORAGE_PROPERTIES, Collections.emptyMap());
        instanceField.set(driver, factoryMock);
    }

    @Test
    public void createsNewConnectionsForAllRequests() throws Exception {
        final Connection cOne = driver.acquireConnection();
        assertNotNull(cOne);
        final Connection cTwo = driver.acquireConnection();
        assertNotNull(cTwo);
        assertNotSame(cOne, cTwo);
    }

    @Test
    public void closesConnectionsOnClose() throws Exception {
        final Connection cOne = driver.acquireConnection();
        assertTrue(cOne.isOpen());
        final Connection cTwo = driver.acquireConnection();
        assertTrue(cTwo.isOpen());

        driver.close();
        assertFalse(cOne.isOpen());
        assertFalse(cTwo.isOpen());
    }

    @Test
    public void reloadDataReloadsDataInConnector() throws Exception {
        driver.reloadData();
        verify(factoryMock).reloadData();
    }
}
