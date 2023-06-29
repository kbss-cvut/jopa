/**
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.config.DriverConfigParam;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.reasoner.rulesys.RDFSRuleReasonerFactory;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.*;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.spy;

public class SnapshotStorageConnectorWithInferenceTest {

    private SharedStorageConnector centralConnector;

    private SnapshotStorageConnectorWithInference connector;

    @BeforeEach
    public void setUp() {
        final DriverConfiguration configuration = StorageTestUtil.createConfiguration("test:uri");
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.centralConnector = spy(new SharedStorageConnector(configuration));
        this.connector = new SnapshotStorageConnectorWithInference(centralConnector, Collections.emptyMap());
    }

    @Test
    public void findReturnsStatementsFromRawDefaultModel() throws Exception {
        generateTestData(null);
        connector.begin();
        final List<Statement> result = connector.find(createResource(SUBJECT), RDF.type, null, Collections.emptySet());
        assertEquals(1, result.size());
        assertEquals(createResource(TYPE_ONE), result.get(0).getObject());
    }

    private void generateTestData(String context) throws JenaDriverException {
        centralConnector.begin();
        final List<Statement> data = Arrays.asList(
                statement(TYPE_ONE, RDFS.subClassOf.getURI(), TYPE_TWO),
                statement(SUBJECT, RDF.type.getURI(), TYPE_ONE));
        centralConnector.add(data, context);
        centralConnector.commit();
    }

    @Test
    public void findWithInferenceReturnsStatementsIncludingInferredKnowledge() throws Exception {
        generateTestData(null);
        connector.begin();
        final Collection<Statement> result = connector
                .findWithInference(createResource(SUBJECT), RDF.type, null, Collections.emptySet());
        assertEquals(2, result.size());
        assertTrue(result.stream().anyMatch(s -> s.getObject().equals(createResource(TYPE_TWO))));
    }

    @Test
    public void findInContextReturnsStatementsFromRawNamedGraph() throws Exception {
        generateTestData(NAMED_GRAPH);
        connector.begin();
        final List<Statement> result = connector
                .find(createResource(SUBJECT), RDF.type, null, Collections.singleton(NAMED_GRAPH));
        assertEquals(1, result.size());
        assertEquals(createResource(TYPE_ONE), result.get(0).getObject());
    }

    @Test
    public void findWithInferenceInContextReturnsStatementsIncludingInferredKnowledge() throws Exception {
        generateTestData(NAMED_GRAPH);
        connector.begin();
        final Collection<Statement> result =
                connector
                        .findWithInference(createResource(SUBJECT), RDF.type, null, Collections.singleton(NAMED_GRAPH));
        assertEquals(2, result.size());
        assertTrue(result.stream().anyMatch(s -> s.getObject().equals(createResource(TYPE_TWO))));
    }

    @Test
    public void containsUsesOnlyRawDefaultGraph() throws Exception {
        generateTestData(null);
        connector.begin();
        assertFalse(connector
                .contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO), Collections.emptySet()));
        assertTrue(connector
                .contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE), Collections.emptySet()));
    }

    @Test
    public void containsWithInferenceUsesInferredKnowledgeInDefaultGraph() throws Exception {
        generateTestData(null);
        connector.begin();
        assertTrue(connector.containsWithInference(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO),
                Collections.emptySet()));
        assertTrue(connector.containsWithInference(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE),
                Collections.emptySet()));
    }

    @Test
    public void containsInContextUsesOnlyRawNamedGraph() throws Exception {
        generateTestData(NAMED_GRAPH);
        connector.begin();
        assertFalse(connector.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO),
                Collections.singleton(NAMED_GRAPH)));
        assertTrue(connector.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE),
                Collections.singleton(NAMED_GRAPH)));
    }

    @Test
    public void containsWithInferenceInContextUsesInferredKnowledgeInNamedGraph() throws Exception {
        generateTestData(NAMED_GRAPH);
        connector.begin();
        assertTrue(connector
                .containsWithInference(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO),
                        Collections.singleton(NAMED_GRAPH)));
        assertTrue(connector
                .containsWithInference(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE),
                        Collections.singleton(NAMED_GRAPH)));
    }

    @Test
    public void addStatementsInfluencesInferenceResults() throws Exception {
        generateTestData(null);
        connector.begin();
        final Resource another = createResource(Generator.generateUri().toString());
        connector.add(Collections.singletonList(statement(another.getURI(), RDF.type.getURI(), TYPE_ONE)), null);
        assertTrue(
                connector.containsWithInference(another, RDF.type, createResource(TYPE_TWO), Collections.emptySet()));
    }

    @Test
    public void removeStatementsInfluencesInferenceResults() throws Exception {
        generateTestData(null);
        connector.begin();
        connector.remove(null, RDF.type, createResource(TYPE_ONE), null);
        assertFalse(connector.containsWithInference(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO),
                Collections.emptySet()));
    }

    @Test
    public void isConsistentVerifiesConsistencyOfDefaultGraph() throws Exception {
        generateTestData(null);
        connector.begin();
        assertTrue(connector.isConsistent(null));
    }

    @Test
    public void isConsistentVerifiesConsistencyOfNamedGraph() throws Exception {
        generateTestData(NAMED_GRAPH);
        connector.begin();
        assertTrue(connector.isConsistent(NAMED_GRAPH));
    }

    @Test
    public void closeAfterRollbackWorksCorrectly() throws Exception {
        generateTestData(null);
        connector.begin();
        connector.rollback();
        connector.close();
        assertFalse(connector.isOpen());
    }

    @Test
    public void containsOnTDBBackedStorageHandlesTransactionalBehavior() throws Exception {
        final File storageDir = Files.createTempDirectory("tdb-test").toFile();
        try {
            final DriverConfiguration config = createConfiguration(storageDir.getAbsolutePath());
            config.setProperty(JenaConfigParam.STORAGE_TYPE, JenaOntoDriverProperties.TDB);
            config.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
            this.centralConnector = new SharedStorageConnector(config);
            this.connector = new SnapshotStorageConnectorWithInference(centralConnector, Collections.emptyMap());
            connector.begin();
            assertFalse(connector.contains(createResource(SUBJECT), RDF.type, null, Collections.emptySet()));
        } finally {
            StorageTestUtil.deleteStorageDir(storageDir);
        }
    }
}
