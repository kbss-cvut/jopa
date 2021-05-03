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
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import cz.cvut.kbss.ontodriver.jena.connector.*;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.reasoner.rulesys.RDFSRuleReasonerFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URI;
import java.nio.file.Files;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.apache.jena.rdf.model.ResourceFactory.createStatement;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

public class JenaDriverTest {

    private OntologyStorageProperties storageProps;
    private final Map<String, String> properties = new HashMap<>();

    private JenaDriver driver;

    @BeforeEach
    public void setUp() {
        this.storageProps = OntologyStorageProperties.driver(JenaDataSource.class.getName())
                                                     .physicalUri(URI.create("temp:memory")).build();
        properties.put(JenaOntoDriverProperties.JENA_STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        properties.put(JenaOntoDriverProperties.JENA_ISOLATION_STRATEGY, JenaOntoDriverProperties.READ_COMMITTED);
    }

    @AfterEach
    public void tearDown() throws Exception {
        if (driver != null && driver.isOpen()) {
            driver.close();
        }
    }

    @Test
    public void initCreatesReadCommittedConnectorFactoryWhenConfigured() throws Exception {
        this.driver = new JenaDriver(storageProps, properties);
        assertNotNull(driver);
        assertTrue(driver.isOpen());
        assertTrue(getConnectorFactory() instanceof ReadCommittedConnectorFactory);
    }

    private ConnectorFactory getConnectorFactory() throws Exception {
        final Field factoryField = JenaDriver.class.getDeclaredField("connectorFactory");
        factoryField.setAccessible(true);
        return (ConnectorFactory) factoryField.get(driver);
    }

    @Test
    public void initCreatesSnapshotConnectorFactoryWhenConfigured() throws Exception {
        properties.put(JenaOntoDriverProperties.JENA_ISOLATION_STRATEGY, JenaOntoDriverProperties.SNAPSHOT);
        this.driver = new JenaDriver(storageProps, properties);
        assertNotNull(driver);
        assertTrue(driver.isOpen());
        assertTrue(getConnectorFactory() instanceof SnapshotConnectorFactory);
    }

    @Test
    public void initCreatesInferenceConnectorFactoryWhenReasonerFactoryIsConfigured() throws Exception {
        properties.put(OntoDriverProperties.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.driver = new JenaDriver(storageProps, properties);
        assertNotNull(driver);
        assertTrue(driver.isOpen());
        assertTrue(getConnectorFactory() instanceof InferenceConnectorFactory);
    }

    @Test
    public void acquireConnectionCreatesAndReturnsConnectionInstance() {
        this.driver = new JenaDriver(storageProps, properties);
        final JenaConnection connection = driver.acquireConnection();
        assertNotNull(connection);
        assertTrue(connection.isOpen());
    }

    @Test
    public void closeClosesAllOpenConnectionsAndConnectorFactory() throws Exception {
        this.driver = new JenaDriver(storageProps, properties);
        final JenaConnection connection = driver.acquireConnection();
        assertTrue(connection.isOpen());
        assertTrue(driver.isOpen());
        driver.close();
        assertFalse(driver.isOpen());
        assertFalse(connection.isOpen());
        assertFalse(getConnectorFactory().isOpen());
    }

    @Test
    public void closingDriverTwiceDoesNothing() throws Exception {
        this.driver = new JenaDriver(storageProps, properties);
        driver.acquireConnection();
        driver.close();
        driver.close();
        assertFalse(driver.isOpen());
    }

    @Test
    public void connectionClosedNotificationRemovesConnectionFromCollectionOfOpenConnections() throws Exception {
        this.driver = new JenaDriver(storageProps, properties);
        final JenaConnection connection = driver.acquireConnection();
        final Field openConnectionsField = JenaDriver.class.getDeclaredField("openConnections");
        openConnectionsField.setAccessible(true);
        final Set openConnections = (Set) openConnectionsField.get(driver);
        assertTrue(openConnections.contains(connection));
        driver.connectionClosed(connection);
        assertFalse(openConnections.contains(connection));
    }

    @Test
    public void driverRegistersItselfAsListenerOnCreatedConnection() throws Exception {
        this.driver = new JenaDriver(storageProps, properties);
        final JenaConnection connection = driver.acquireConnection();
        final Field listenerField = JenaConnection.class.getDeclaredField("listener");
        listenerField.setAccessible(true);
        assertEquals(driver, listenerField.get(connection));
    }

    @Test
    public void driverCreatesAutoCommitConnectionsWhenConfiguredTo() {
        properties.put(OntoDriverProperties.CONNECTION_AUTO_COMMIT, Boolean.TRUE.toString());
        this.driver = new JenaDriver(storageProps, properties);
        final JenaConnection connection = driver.acquireConnection();
        assertTrue(connection.isAutoCommit());
    }

    @Test
    public void reloadStorageReloadsUnderlyingStorage() throws Exception {
        properties.put(JenaOntoDriverProperties.JENA_STORAGE_TYPE, JenaOntoDriverProperties.FILE);
        final File storage = Files.createTempFile("jena-driver-test", ".ttl").toFile();
        storage.deleteOnExit();
        this.storageProps = OntologyStorageProperties.driver(JenaDataSource.class.getName())
                                                     .physicalUri(storage.toURI()).build();
        this.driver = new JenaDriver(storageProps, properties);

        final JenaConnection con = driver.acquireConnection();
        final PreparedStatement statement = con.prepareStatement("SELECT * WHERE { ?x ?y ?z .}");
        final ResultSet before = statement.executeQuery();
        assertFalse(before.hasNext());
        before.close();
        generateData(storage);

        driver.reloadStorage();
        final ResultSet after = statement.executeQuery();
        assertTrue(after.hasNext());
        after.close();
    }

    private void generateData(File storage) throws IOException {
        final Model model = RDFDataMgr.loadModel(storage.getAbsolutePath());
        model.add(createStatement(createResource(Generator.generateUri().toString()), RDF.type,
                createResource(Generator.generateUri().toString())));
        try (final BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(storage))) {
            RDFDataMgr.write(out, model, Lang.TTL);
        }
    }

    @Test
    public void acquireConnectionThrowsIllegalStateExceptionWhenDriverIsClosed() throws Exception {
        this.driver = new JenaDriver(storageProps, properties);
        driver.close();

        final IllegalStateException ex = assertThrows(IllegalStateException.class, () -> driver.acquireConnection());
        assertThat(ex.getMessage(), containsString("Driver is closed"));
    }

    @Test
    public void setDatasetReplacesUnderlyingDataset() throws Exception {
        this.driver = new JenaDriver(storageProps, properties);
        assertNotNull(driver);
        final Dataset newDataset = DatasetFactory.createTxnMem();
        final URI subject = Generator.generateUri();
        final URI type = Generator.generateUri();
        newDataset.getDefaultModel()
                  .add(createStatement(createResource(subject.toString()), RDF.type, createResource(type.toString())));
        driver.setDataset(newDataset);

        final JenaConnection connection = driver.acquireConnection();
        final Set<Axiom<URI>> types = connection.types()
                                                .getTypes(NamedResource.create(subject), Collections.emptySet(), false);
        assertEquals(1, types.size());
        assertEquals(type, types.iterator().next().getValue().getValue());
    }

    @Test
    public void setDatasetThrowsJenaDriverExceptionWhenTryingToReplaceDatasetInTransaction() throws Exception {
        this.driver = new JenaDriver(storageProps, properties);
        assertNotNull(driver);
        final JenaConnection connection = driver.acquireConnection();
        final SharedStorageConnector centralConnector = connection.unwrap(SharedStorageConnector.class);
        centralConnector.begin();
        final JenaDriverException ex = assertThrows(JenaDriverException.class,
                () -> driver.setDataset(DatasetFactory.create()));
        assertThat(ex.getCause(), instanceOf(IllegalStateException.class));
    }
}
