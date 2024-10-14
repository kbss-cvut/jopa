package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.query.Dataset;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.tdb2.TDB2Factory;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.RESOURCE;
import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.TYPE_ONE;
import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.createConfiguration;
import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.generateTestData;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TDB2StorageTest {

    private File storageDir;

    private TDB2Storage sut;

    @AfterEach
    public void tearDown() throws IOException {
        if (storageDir != null) {
            StorageTestUtil.deleteStorageDir(storageDir);
        }
    }

    @Test
    public void initializeCreatesNewTDB2Storage() throws Exception {
        final File storageDir = initStorage();
        assertTrue(storageDir.exists());
        final File[] files = storageDir.listFiles();
        assertNotNull(files);
        assertTrue(files.length > 0);
    }

    private File initStorage() throws IOException {
        this.storageDir = Files.createTempDirectory("tdb2-test").toFile();
        storageDir.deleteOnExit();
        this.sut = new TDB2Storage(createConfiguration(storageDir.getAbsolutePath()));
        return storageDir;
    }

    @Test
    public void initializeLoadsExistingTDB2Dataset() throws Exception {
        this.storageDir = Files.createTempDirectory("tdb2-test").toFile();
        storageDir.deleteOnExit();
        final Dataset dataset = TDB2Factory.connectDataset(storageDir.getAbsolutePath());
        dataset.begin(ReadWrite.WRITE);
        generateTestData(dataset);
        dataset.commit();
        dataset.close();

        this.sut = new TDB2Storage(createConfiguration(storageDir.getAbsolutePath()));
        sut.begin(ReadWrite.READ);
        assertTrue(sut.getDataset().getDefaultModel().contains(RESOURCE, RDF.type, createResource(TYPE_ONE)));
        sut.commit();
    }

    @Test
    public void initializeCreatesNewTDBStorageWhenTargetDirectoryDoesNotExist() throws Exception {
        this.storageDir = Files.createTempDirectory("tdb2-test").toFile();
        storageDir.deleteOnExit();
        storageDir.delete();
        assertFalse(storageDir.exists());
        this.sut = new TDB2Storage(createConfiguration(storageDir.getAbsolutePath()));
        assertTrue(storageDir.exists());
    }

    @Test
    public void writeChangesSynchronizesDatasetToStorage() throws Exception {
        final File storageDir = initStorage();
        sut.begin(ReadWrite.WRITE);
        generateTestData(sut.getDataset());
        sut.commit();
        sut.writeChanges();
        sut.close();

        final Dataset result = TDB2Factory.connectDataset(storageDir.getAbsolutePath());
        result.begin(ReadWrite.READ);
        assertTrue(result.getDefaultModel().contains(RESOURCE, RDF.type, createResource(TYPE_ONE)));
        result.abort();
        result.close();
    }
}
