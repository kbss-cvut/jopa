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
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfigParam;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.ReasonerInitializationException;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.*;
import org.apache.jena.reasoner.InfGraph;
import org.apache.jena.reasoner.Reasoner;
import org.apache.jena.reasoner.ReasonerFactory;
import org.apache.jena.reasoner.ValidityReport;
import org.apache.jena.reasoner.rulesys.*;
import org.apache.jena.tdb.TDBFactory;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.apache.jena.vocabulary.ReasonerVocabulary;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.*;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

public class SnapshotStorageWithInferenceTest {

    private DriverConfiguration configuration;

    private SnapshotStorageWithInference storage;

    @BeforeEach
    public void setUp() {
        this.configuration = createConfiguration("urn:storage-test");
    }

    @Test
    public void initializationCreatesRDFSReasonerBasedOnConfiguredReasonerFactoryClass() throws Exception {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
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
                .setProperty(DriverConfigParam.REASONER_FACTORY_CLASS,
                        "cz.cvut.kbss.ontodriver.jena.UnknownReasonerFactory");
        final ReasonerInitializationException ex = assertThrows(ReasonerInitializationException.class,
                () -> new SnapshotStorageWithInference(configuration, Collections.emptyMap()));
        assertThat(ex.getCause(), instanceOf(ClassNotFoundException.class));
        assertThat(ex.getMessage(), containsString("Reasoner factory class"));
        assertThat(ex.getMessage(), containsString("not found"));
    }

    @Test
    public void initializationThrowsReasonerInitializationExceptionWhenClassIsNotReasonerFactory() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, String.class.getName());
        final ReasonerInitializationException ex = assertThrows(ReasonerInitializationException.class,
                () -> new SnapshotStorageWithInference(configuration, Collections.emptyMap()));
        assertThat(ex.getMessage(),
                containsString("Class " + String.class.getName() + " is not a ReasonerFactory implementation"));
    }

    @Test
    public void initializationCreatesOWLReasonerBasedOnConfiguredReasonerFactoryClass() throws Exception {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, OWLFBRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
        assertTrue(getReasonerFactory() instanceof OWLFBRuleReasonerFactory);
    }

    @Test
    public void initializationEagerlyCreatesInferredModelForDefaultGraph() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
        storage.addCentralData(getDatasetWithDefaultModel());
        assertTrue(storage.dataset.getDefaultModel().getGraph() instanceof InfGraph);
    }

    @Test
    public void getDefaultGraphReturnsInferredDefaultGraph() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
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
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
        storage.addCentralData(getDatasetWithDefaultModel());
        final Model result = storage.getDefaultGraph();
        assertEquals(result, storage.getDefaultGraph());
    }

    @Test
    public void getRawDefaultModelReturnsDefaultGraphWithoutInference() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
        storage.addCentralData(getDatasetWithDefaultModel());
        final Model result = storage.getRawDefaultGraph();
        assertTrue(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE)));
        assertFalse(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void getRawDefaultModelReturnsDefaultGraphWithoutInferenceWhenInferredModelExists() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
        storage.addCentralData(getDatasetWithDefaultModel());
        assertTrue(storage.getDefaultGraph().contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
        final Model result = storage.getRawDefaultGraph();
        assertFalse(result instanceof InfModel);
        assertTrue(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE)));
        assertFalse(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void initializationEagerlyCreatesInfModelForNamedGraphsFromCentral() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
        storage.addCentralData(getDatasetWithDataInNamedGraph());
        assertTrue(storage.getDataset().getNamedModel(NAMED_GRAPH).getGraph() instanceof InfGraph);
    }

    @Test
    public void getNamedGraphReturnsModelWithInferences() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
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
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
        storage.addCentralData(getDatasetWithDataInNamedGraph());
        final Model first = storage.getNamedGraph(NAMED_GRAPH);
        assertEquals(first, storage.getNamedGraph(NAMED_GRAPH));
    }

    @Test
    public void getRawNamedGraphReturnsGraphWithoutInference() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
        storage.addCentralData(getDatasetWithDataInNamedGraph());
        final Model result = storage.getRawNamedGraph(NAMED_GRAPH);
        assertTrue(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE)));
        assertFalse(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void getRawNamedGraphReturnsGraphWithoutInferenceWhenInferredAlreadyExists() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
        storage.addCentralData(getDatasetWithDataInNamedGraph());
        assertTrue(storage.getNamedGraph(NAMED_GRAPH)
                          .contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
        final Model result = storage.getRawNamedGraph(NAMED_GRAPH);
        assertTrue(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE)));
        assertFalse(result.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void checkConsistencyReturnsValidityReportForDefaultGraph() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
        storage.addCentralData(getDatasetWithDefaultModel());
        final ValidityReport result = storage.checkConsistency(null);
        assertNotNull(result);
        assertTrue(result.isValid());
    }

    @Test
    public void checkConsistencyReturnsValidityReportForNamedGraph() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
        storage.addCentralData(getDatasetWithDataInNamedGraph());
        final ValidityReport result = storage.checkConsistency(NAMED_GRAPH);
        assertNotNull(result);
        assertTrue(result.isValid());
    }

    @Test
    public void checkConsistencyReturnsValidityReportForInconsistentDefaultGraph() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, OWLMiniReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
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

    @Test
    public void configurationIsPassedToReasonerOnCreation() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        final Map<String, String> config =
                Collections.singletonMap(ReasonerVocabulary.PROPtraceOn.getURI(), Boolean.TRUE.toString());
        this.storage = new SnapshotStorageWithInference(configuration, config);
        storage.addCentralData(getDatasetWithDefaultModel());
        final InfModel infModel = storage.getDefaultGraph();
        final Reasoner reasoner = infModel.getReasoner();
        assertTrue(((RDFSRuleReasoner) reasoner).isTraceOn());
    }

    @Test
    public void configurationSkipsUnsupportedConfigurationParameters() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        final Map<String, String> config = new HashMap<>(4);
        config.put(JenaOntoDriverProperties.JENA_ISOLATION_STRATEGY, JenaOntoDriverProperties.SNAPSHOT);
        config.put(JenaOntoDriverProperties.JENA_STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        this.storage = new SnapshotStorageWithInference(configuration, config);
        storage.addCentralData(getDatasetWithDefaultModel());
        final InfModel infModel = storage.getDefaultGraph();
        final Reasoner reasoner = infModel.getReasoner();
        assertFalse(((RDFSRuleReasoner) reasoner).isTraceOn());
    }

    @Test
    public void configurationSkipsParametersNotSupportedByParticularReasoner() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, GenericRuleReasonerFactory.class.getName());
        final Map<String, String> config = new HashMap<>(4);
        config.put(ReasonerVocabulary.PROPtraceOn.getURI(), Boolean.TRUE.toString());
        // This is not supported by the RDFSRuleReasoner
        config.put(ReasonerVocabulary.PROPruleMode.getURI(), "yadayada");
        this.storage = new SnapshotStorageWithInference(configuration, config);
        storage.addCentralData(getDatasetWithDefaultModel());
        final InfModel infModel = storage.getDefaultGraph();
        final Reasoner reasoner = infModel.getReasoner();
        assertTrue(((GenericRuleReasoner) reasoner).isTraceOn());
    }

    @Test
    public void initializationFromCentralConnectorCreatesIndependentDefaultGraph() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
        final Dataset central = getDatasetWithDefaultModel();
        storage.addCentralData(central);
        final Model result = storage.getRawDefaultGraph();
        result.add(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO));
        assertFalse(central.getDefaultModel().contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void initializationFromCentralConnectorCreatesIndependentNamedGraphs() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
        final Dataset central = getDatasetWithDataInNamedGraph();
        storage.addCentralData(central);
        final Model result = storage.getRawNamedGraph(NAMED_GRAPH);
        result.add(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO));
        assertFalse(central.getNamedModel(NAMED_GRAPH)
                           .contains(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void initializationFromCentralTDBConnectorSupportsNonTransactionalModels() throws Exception {
        final File storageDir = Files.createTempDirectory("tdb-test").toFile();
        try {
            final Dataset tdbDataset = TDBFactory.createDataset(storageDir.getAbsolutePath());
            tdbDataset.begin(ReadWrite.WRITE);
            generateTestData(tdbDataset);
            tdbDataset.commit();
            configuration
                    .setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
            this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
            storage.addCentralData(tdbDataset);
            final Model defaultGraph = storage.getDefaultGraph();
            assertTrue(defaultGraph.contains(createResource(SUBJECT), RDF.type, (RDFNode) null));
        } finally {
            StorageTestUtil.deleteStorageDir(storageDir);
        }
    }

    @Test
    public void getNamedModelReturnsInfModelAlsoForUnknownContextName() {
        configuration.setProperty(DriverConfigParam.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.storage = new SnapshotStorageWithInference(configuration, Collections.emptyMap());
        storage.addCentralData(getDatasetWithDefaultModel());
        // Does not exist
        final InfModel result = storage.getNamedGraph(NAMED_GRAPH);
        assertNotNull(result);
    }
}
