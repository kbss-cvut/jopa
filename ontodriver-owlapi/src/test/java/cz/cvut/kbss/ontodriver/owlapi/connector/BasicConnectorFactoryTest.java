package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
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
import java.util.Collections;

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
        storageProperties = OntologyStorageProperties.connectorType(OntologyConnectorType.OWLAPI)
                                                     .ontologyUri(ontologyUri).physicalUri(physicalUri).build();
    }

    @Before
    public void setUp() throws Exception {
        this.factory = new BasicConnectorFactory();
    }

    @Test
    public void getConnectorReturnsTheSameCentralConnectorForAllCalls() throws Exception {
        final Connector connectorOne = factory.getConnector(storageProperties, Collections.emptyMap());
        assertNotNull(connectorOne);
        final Connector connectorTwo = factory.getConnector(storageProperties, Collections.emptyMap());
        assertNotNull(connectorTwo);
        assertSame(connectorOne, connectorTwo);
    }

    @Test
    public void closesConnectorOnFactoryClose() throws Exception {
        final AbstractConnector connector = factory.getConnector(storageProperties, Collections.emptyMap());
        assertTrue(connector.isOpen());
        factory.close();
        assertFalse(connector.isOpen());
    }
}
