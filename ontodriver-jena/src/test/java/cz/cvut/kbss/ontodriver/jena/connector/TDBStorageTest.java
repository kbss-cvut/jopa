package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.query.Dataset;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.tdb.TDB;
import org.apache.jena.tdb.TDBFactory;
import org.apache.jena.vocabulary.RDF;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.*;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.junit.Assert.*;

public class TDBStorageTest {

    private TDBStorage storage;

    @Test
    public void initializeCreatesNewTDBStorage() throws Exception {
        final File storageDir = initTDBStorage();
        assertTrue(storageDir.exists());
        final File[] files = storageDir.listFiles();
        assertNotNull(files);
        assertTrue(files.length > 0);
    }

    private File initTDBStorage() throws IOException {
        final File storageDir = Files.createTempDirectory("tdb-test").toFile();
        storageDir.deleteOnExit();
        this.storage = new TDBStorage(createConfiguration(storageDir.getAbsolutePath()));
        storage.initialize();
        return storageDir;
    }

    @Test
    public void initializeLoadsExistingTDBDataset() throws Exception {
        final File storageDir = Files.createTempDirectory("tdb-test").toFile();
        storageDir.deleteOnExit();
        final Dataset dataset = TDBFactory.createDataset(storageDir.getAbsolutePath());
        dataset.begin(ReadWrite.WRITE);
        generateTestData(dataset);
        dataset.commit();
        TDB.sync(dataset);
        dataset.close();

        this.storage = new TDBStorage(createConfiguration(storageDir.getAbsolutePath()));
        storage.initialize();
        storage.begin(ReadWrite.READ);
        assertTrue(storage.getDataset().getDefaultModel().contains(RESOURCE, RDF.type, createResource(TYPE_ONE)));
        storage.commit();
    }

    @Test
    public void initializeCreatesNewTDBStorageWhenTargetDirectoryDoesNotExist() throws Exception {
        final File storageDir = Files.createTempDirectory("tdb-test").toFile();
        storageDir.deleteOnExit();
        storageDir.delete();
        assertFalse(storageDir.exists());
        this.storage = new TDBStorage(createConfiguration(storageDir.getAbsolutePath()));
        storage.initialize();
        assertTrue(storageDir.exists());
    }

    @Test
    public void writeChangesSynchronizesDatasetToStorage() throws Exception {
        final File storageDir = initTDBStorage();
        storage.begin(ReadWrite.WRITE);
        generateTestData(storage.getDataset());
        storage.commit();
        storage.writeChanges();

        final Dataset result = TDBFactory.createDataset(storageDir.getAbsolutePath());
        result.begin(ReadWrite.READ);
        assertTrue(result.getDefaultModel().contains(RESOURCE, RDF.type, createResource(TYPE_ONE)));
        result.abort();
        result.close();
    }
}