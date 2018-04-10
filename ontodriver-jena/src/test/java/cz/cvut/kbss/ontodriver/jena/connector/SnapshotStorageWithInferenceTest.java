package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.ConfigParam;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.ReasonerInitializationException;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.rdf.model.InfModel;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.reasoner.Reasoner;
import org.apache.jena.reasoner.ReasonerFactory;
import org.apache.jena.reasoner.ValidityReport;
import org.apache.jena.reasoner.rulesys.OWLFBRuleReasonerFactory;
import org.apache.jena.reasoner.rulesys.OWLMiniReasonerFactory;
import org.apache.jena.reasoner.rulesys.RDFSRuleReasonerFactory;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.lang.reflect.Field;

import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.*;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.isA;
import static org.junit.Assert.*;

public class SnapshotStorageWithInferenceTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    private Configuration configuration;

    private SnapshotStorageWithInference storage;

    @Before
    public void setUp() {
        this.configuration = createConfiguration("urn:storage-test");
    }

    @Test
    public void initializationCreatesRDFSReasonerBasedOnConfiguredReasonerFactoryClass() throws Exception {
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration);
        assertTrue(getReasonerFactory() instanceof RDFSRuleReasonerFactory);
    }

    private ReasonerFactory getReasonerFactory() throws Exception {
        final Field rfField = SnapshotStorageWithInference.class.getDeclaredField("reasonerFactory");
        rfField.setAccessible(true);
        return (ReasonerFactory) rfField.get(storage);
    }

    @Test
    public void initializationThrowsReasonerInitializationExceptionWhenUnknownReasonerFactoryClassIsSpecified() {
        configuration
                .setProperty(ConfigParam.REASONER_FACTORY_CLASS, "cz.cvut.kbss.ontodriver.jena.UnknownReasonerFactory");
        thrown.expect(ReasonerInitializationException.class);
        thrown.expectCause(isA(ClassNotFoundException.class));
        thrown.expectMessage(containsString("Reasoner factory class"));
        thrown.expectMessage(containsString("not found"));
        new SnapshotStorageWithInference(configuration);
    }

    @Test
    public void initializationThrowsReasonerInitializationExceptionWhenClassIsNotReasonerFactory() {
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, String.class.getName());
        thrown.expect(ReasonerInitializationException.class);
        thrown.expectMessage(
                containsString("Class " + String.class.getName() + " is not a ReasonerFactory implementation"));
        new SnapshotStorageWithInference(configuration);
    }

    @Test
    public void initializationCreatesOWLReasonerBasedOnConfiguredReasonerFactoryClass() throws Exception {
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, OWLFBRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration);
        assertTrue(getReasonerFactory() instanceof OWLFBRuleReasonerFactory);
    }

    @Test
    public void getDefaultGraphReturnsInferredDefaultGraph() {
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration);
        storage.initialize();
        storage.addCentralData(getDatasetWithDefaultModel());
        final Model result = storage.getDefaultGraph();
        assertNotNull(result);
        assertTrue(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
    }

    private Dataset getDatasetWithDefaultModel() {
        final Dataset ds = DatasetFactory.createTxnMem();
        ds.setDefaultModel(getModelForInference());
        return ds;
    }

    private Model getModelForInference() {
        final Model model = ModelFactory.createDefaultModel();
        model.add(statement(TYPE_ONE, RDFS.subClassOf.getURI(), TYPE_TWO));
        model.add(statement(SUBJECT, RDF.type.getURI(), TYPE_ONE));
        return model;
    }

    @Test
    public void getDefaultGraphReusesInfModelAfterFirstCall() {
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration);
        storage.initialize();
        storage.addCentralData(getDatasetWithDefaultModel());
        final Model result = storage.getDefaultGraph();
        assertEquals(result, storage.getDefaultGraph());
    }

    @Test
    public void getRawDefaultModelReturnsDefaultGraphWithoutInference() {
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration);
        storage.initialize();
        storage.addCentralData(getDatasetWithDefaultModel());
        final Model result = storage.getRawDefaultGraph();
        assertTrue(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE)));
        assertFalse(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void getRawDefaultModelReturnsDefaultGraphWithoutInferenceWhenInferredModelExists() {
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration);
        storage.initialize();
        storage.addCentralData(getDatasetWithDefaultModel());
        assertTrue(storage.getDefaultGraph().contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
        final Model result = storage.getRawDefaultGraph();
        assertFalse(result instanceof InfModel);
        assertTrue(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE)));
        assertFalse(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void getNamedGraphReturnsModelWithInferences() {
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration);
        storage.initialize();
        storage.addCentralData(getDatasetWithDataInNamedGraph());
        final Model result = storage.getNamedGraph(NAMED_GRAPH);
        assertTrue(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
    }

    private Dataset getDatasetWithDataInNamedGraph() {
        final Dataset dataset = DatasetFactory.createTxnMem();
        dataset.addNamedModel(NAMED_GRAPH, getModelForInference());
        return dataset;
    }

    @Test
    public void getNamedGraphReusesInfModelAfterFirstCall() {
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration);
        storage.initialize();
        storage.addCentralData(getDatasetWithDataInNamedGraph());
        final Model first = storage.getNamedGraph(NAMED_GRAPH);
        assertEquals(first, storage.getNamedGraph(NAMED_GRAPH));
    }

    @Test
    public void getRawNamedGraphReturnsGraphWithoutInference() {
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration);
        storage.initialize();
        storage.addCentralData(getDatasetWithDataInNamedGraph());
        final Model result = storage.getRawNamedGraph(NAMED_GRAPH);
        assertTrue(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE)));
        assertFalse(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void getRawNamedGraphReturnsGraphWithoutInferenceWhenInferredAlreadyExists() {
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration);
        storage.initialize();
        storage.addCentralData(getDatasetWithDataInNamedGraph());
        assertTrue(storage.getNamedGraph(NAMED_GRAPH)
                          .contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
        final Model result = storage.getRawNamedGraph(NAMED_GRAPH);
        assertTrue(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE)));
        assertFalse(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void checkConsistencyReturnsValidityReportForDefaultGraph() {
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration);
        storage.initialize();
        storage.addCentralData(getDatasetWithDefaultModel());
        final ValidityReport result = storage.checkConsistency(null);
        assertNotNull(result);
        assertTrue(result.isValid());
    }

    @Test
    public void checkConsistencyReturnsValidityReportForNamedGraph() {
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration);
        storage.initialize();
        storage.addCentralData(getDatasetWithDataInNamedGraph());
        final ValidityReport result = storage.checkConsistency(NAMED_GRAPH);
        assertNotNull(result);
        assertTrue(result.isValid());
    }

    @Test
    public void checkConsistencyReturnsValidityReportForInconsistentDefaultGraph() {
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, OWLMiniReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration);
        storage.initialize();
        storage.addCentralData(getInconsistentDataset());
        final ValidityReport result = storage.checkConsistency(null);
        assertNotNull(result);
        assertFalse(result.isValid());
    }

    private Dataset getInconsistentDataset() {
        final Model model = ModelFactory.createOntologyModel();
        final Resource clsOne = createResource(Generator.generateUri().toString());
        final Resource clsTwo = createResource(Generator.generateUri().toString());
        model.add(clsOne, RDF.type, RDFS.Class);
        model.add(clsTwo, RDF.type, RDFS.Class);
        model.add(clsOne, OWL.disjointWith, clsTwo);
        model.add(createResource(SUBJECT), RDF.type, clsOne);
        model.add(createResource(SUBJECT), RDF.type, clsTwo);
        return DatasetFactory.create(model);
    }

    @Ignore
    @Test
    public void supportedConfigurationIsPassedToReasonerOnCreation() {
        configuration.setProperty(ConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration);
        storage.initialize();
        storage.addCentralData(getDatasetWithDefaultModel());
        final InfModel result = storage.getDefaultGraph();
        final Reasoner reasoner = result.getReasoner();
        // TODO
    }
}