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
package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiDataSource;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import java.io.File;
import java.net.URI;
import java.nio.file.Files;

import static org.junit.Assert.*;

public class BasicConnectorFactoryTest {

    private static OntologyStorageProperties storageProperties;

    private BasicConnectorFactory factory;

    @BeforeClass
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

    @Before
    public void setUp() throws Exception {
        this.factory = new BasicConnectorFactory();
    }

    @Test
    public void getConnectorReturnsTheSameCentralConnectorForAllCalls() throws Exception {
        final Connector connectorOne = factory.getConnector(new Configuration(storageProperties));
        assertNotNull(connectorOne);
        final Connector connectorTwo = factory.getConnector(new Configuration(storageProperties));
        assertNotNull(connectorTwo);
        assertSame(connectorOne, connectorTwo);
    }

    @Test
    public void closesConnectorOnFactoryClose() throws Exception {
        final AbstractConnector connector = factory.getConnector(new Configuration(storageProperties));
        assertTrue(connector.isOpen());
        factory.close();
        assertFalse(connector.isOpen());
    }
}
