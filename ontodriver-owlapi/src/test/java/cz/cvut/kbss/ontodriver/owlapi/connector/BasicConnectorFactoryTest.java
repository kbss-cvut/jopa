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
package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiDataSource;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import java.io.File;
import java.lang.reflect.Field;
import java.net.URI;
import java.nio.file.Files;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

public class BasicConnectorFactoryTest {

    private static OntologyStorageProperties storageProperties;

    private BasicConnectorFactory factory;

    @BeforeAll
    public static void setUpBeforeClass() throws Exception {
        final URI ontologyUri = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/ConnectorFactoryTest");
        final File targetFile = Files.createTempFile("connectortest", ".owl").toFile();
        targetFile.deleteOnExit();
        final OWLOntologyManager om = OWLManager.createOWLOntologyManager();
        final OWLOntology o = om.createOntology(IRI.create(ontologyUri));
        om.saveOntology(o, IRI.create(targetFile));
        final URI physicalUri = targetFile.toURI();
        storageProperties = OntologyStorageProperties.ontologyUri(ontologyUri).physicalUri(physicalUri).driver(
                OwlapiDataSource.class.getCanonicalName()).build();
    }

    @BeforeEach
    public void setUp() {
        this.factory = new BasicConnectorFactory();
    }

    @Test
    public void getConnectorReturnsTheSameCentralConnectorForAllCalls() throws Exception {
        final Connector connectorOne = factory.getConnector(new DriverConfiguration(storageProperties));
        assertNotNull(connectorOne);
        final Connector connectorTwo = factory.getConnector(new DriverConfiguration(storageProperties));
        assertNotNull(connectorTwo);
        assertSame(connectorOne, connectorTwo);
    }

    @Test
    public void closesConnectorOnFactoryClose() throws Exception {
        final AbstractConnector connector = factory.getConnector(new DriverConfiguration(storageProperties));
        assertTrue(connector.isOpen());
        factory.close();
        assertFalse(connector.isOpen());
    }

    @Test
    public void getConnectorOnCloseFactoryThrowsIllegalStateException() throws Exception {
        final DriverConfiguration config = new DriverConfiguration(storageProperties);
        assertTrue(factory.isOpen());
        factory.getConnector(config);
        factory.close();
        assertFalse(factory.isOpen());
        assertThrows(IllegalStateException.class, () -> factory.getConnector(config));
    }

    @Test
    public void reloadDataReloadsDataOnSharedConnector() throws Exception {
        final AbstractConnector connector = spy(factory.getConnector(new DriverConfiguration(storageProperties)));
        final Field connectorField = BasicConnectorFactory.class.getDeclaredField("connector");
        connectorField.setAccessible(true);
        connectorField.set(factory, connector);
        factory.reloadData();
        verify(connector).reloadData();
    }

    @Test
    public void reloadDataDoesNothingWhenConnectorDoesNotYetExist() throws Exception {
        final Field connectorField = BasicConnectorFactory.class.getDeclaredField("connector");
        connectorField.setAccessible(true);
        assertNull(connectorField.get(factory));
        factory.reloadData();
        assertNull(connectorField.get(factory));
    }
}
