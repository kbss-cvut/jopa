package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.query.Dataset;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.tdb.TDBFactory;
import org.apache.jena.vocabulary.RDF;
import org.junit.After;
import org.junit.Test;

import java.io.File;
import java.nio.file.Files;

import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.*;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.junit.Assert.assertTrue;

public class SnapshotStorageTest {

    private File storageDir;

    @After
    public void tearDown() {
        if (storageDir != null) {
            StorageTestUtil.deleteStorageDir(storageDir);
        }
    }

    @Test
    public void initializationFromCentralTDBConnectorSupportsNonTransactionalModels() throws Exception {
        this.storageDir = Files.createTempDirectory("tdb-test").toFile();
        storageDir.deleteOnExit();
        final Dataset tdbDataset = TDBFactory.createDataset(storageDir.getAbsolutePath());
        tdbDataset.begin(ReadWrite.WRITE);
        generateTestData(tdbDataset);
        tdbDataset.commit();
        final SnapshotStorage storage = new SnapshotStorage(createConfiguration(storageDir.getAbsolutePath()));
        storage.initialize();
        storage.addCentralData(tdbDataset);
        final Model defaultGraph = storage.getDefaultGraph();
        assertTrue(defaultGraph.contains(createResource(SUBJECT), RDF.type, (RDFNode) null));
    }
}