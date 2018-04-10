package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.ConfigParam;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.reasoner.rulesys.RDFSRuleReasonerFactory;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.*;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.junit.Assert.*;
import static org.mockito.Mockito.spy;

public class SnapshotStorageConnectorWithInferenceTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    private SharedStorageConnector centralConnector;

    private SnapshotStorageConnectorWithInference connector;

    @Before
    public void setUp() {
        final Configuration configuration = StorageTestUtil.createConfiguration("test:uri");
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.centralConnector = spy(new SharedStorageConnector(configuration));
        this.connector = new SnapshotStorageConnectorWithInference(centralConnector);
    }

    @Test
    public void findReturnsStatementsFromRawDefaultModel() throws Exception {
        generateTestData(null);
        connector.begin();
        final List<Statement> result = connector.find(createResource(SUBJECT), RDF.type, null);
        assertEquals(1, result.size());
        assertEquals(createResource(TYPE_ONE), result.get(0).getObject());
    }

    private void generateTestData(String context) throws JenaDriverException {
        centralConnector.begin();
        final List<Statement> data = Arrays.asList(
                statement(TYPE_ONE, RDFS.subClassOf.getURI(), TYPE_TWO),
                statement(SUBJECT, RDF.type.getURI(), TYPE_ONE));
        if (context != null) {
            centralConnector.add(data, context);
        } else {
            centralConnector.add(data);
        }
        centralConnector.commit();
    }

    @Test
    public void findWithInferenceReturnsStatementsIncludingInferredKnowledge() throws Exception {
        generateTestData(null);
        connector.begin();
        final Collection<Statement> result = connector.findWithInference(createResource(SUBJECT), RDF.type, null);
        assertEquals(2, result.size());
        assertTrue(result.stream().anyMatch(s -> s.getObject().equals(createResource(TYPE_TWO))));
    }

    @Test
    public void findInContextReturnsStatementsFromRawNamedGraph() throws Exception {
        generateTestData(NAMED_GRAPH);
        connector.begin();
        final List<Statement> result = connector.find(createResource(SUBJECT), RDF.type, null, NAMED_GRAPH);
        assertEquals(1, result.size());
        assertEquals(createResource(TYPE_ONE), result.get(0).getObject());
    }

    @Test
    public void findWithInferenceInContextReturnsStatementsIncludingInferredKnowledge() throws Exception {
        generateTestData(NAMED_GRAPH);
        connector.begin();
        final Collection<Statement> result =
                connector.findWithInference(createResource(SUBJECT), RDF.type, null, NAMED_GRAPH);
        assertEquals(2, result.size());
        assertTrue(result.stream().anyMatch(s -> s.getObject().equals(createResource(TYPE_TWO))));
    }

    @Test
    public void containsUsesOnlyRawDefaultGraph() throws Exception {
        generateTestData(null);
        connector.begin();
        assertFalse(connector.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
        assertTrue(connector.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE)));
    }

    @Test
    public void containsWithInferenceUsesInferredKnowledgeInDefaultGraph() throws Exception {
        generateTestData(null);
        connector.begin();
        assertTrue(connector.containsWithInference(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
        assertTrue(connector.containsWithInference(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE)));
    }

    @Test
    public void containsInContextUsesOnlyRawNamedGraph() throws Exception {
        generateTestData(NAMED_GRAPH);
        connector.begin();
        assertFalse(connector.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO), NAMED_GRAPH));
        assertTrue(connector.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE), NAMED_GRAPH));
    }

    @Test
    public void containsWithInferenceInContextUsesInferredKnowledgeInNamedGraph() throws Exception {
        generateTestData(NAMED_GRAPH);
        connector.begin();
        assertTrue(connector
                .containsWithInference(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO), NAMED_GRAPH));
        assertTrue(connector
                .containsWithInference(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE), NAMED_GRAPH));
    }

    @Test
    public void addStatementsInfluencesInferenceResults() throws Exception {
        generateTestData(null);
        connector.begin();
        final Resource another = createResource(Generator.generateUri().toString());
        connector.add(Collections.singletonList(statement(another.getURI(), RDF.type.getURI(), TYPE_ONE)));
        assertTrue(connector.containsWithInference(another, RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void removeStatementsInfluencesInferenceResults() throws Exception {
        generateTestData(null);
        connector.begin();
        connector.remove(null, RDF.type, createResource(TYPE_ONE));
        assertFalse(connector.containsWithInference(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void isConsistentVerifiesConsistencyOfDefaultGraph() throws Exception {
        generateTestData(null);
        connector.begin();
        assertTrue(connector.isConsistent());
    }

    @Test
    public void isConsistentVerifiesConsistencyOfNamedGraph() throws Exception {
        generateTestData(NAMED_GRAPH);
        connector.begin();
        assertTrue(connector.isConsistent(NAMED_GRAPH));
    }
}