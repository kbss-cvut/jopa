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

import org.apache.jena.query.Dataset;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.tdb.TDB;
import org.apache.jena.tdb.TDBFactory;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.*;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.junit.jupiter.api.Assertions.*;

public class TDBStorageTest {

    private TDBStorage storage;

    private File storageDir;

    @AfterEach
    public void tearDown() {
        if (storageDir != null) {
            StorageTestUtil.deleteStorageDir(storageDir);
        }
    }

    @Test
    public void initializeCreatesNewTDBStorage() throws Exception {
        final File storageDir = initTDBStorage();
        assertTrue(storageDir.exists());
        final File[] files = storageDir.listFiles();
        assertNotNull(files);
        assertTrue(files.length > 0);
    }

    private File initTDBStorage() throws IOException {
        this.storageDir = Files.createTempDirectory("tdb-test").toFile();
        storageDir.deleteOnExit();
        this.storage = new TDBStorage(createConfiguration(storageDir.getAbsolutePath()));
        return storageDir;
    }

    @Test
    public void initializeLoadsExistingTDBDataset() throws Exception {
        this.storageDir = Files.createTempDirectory("tdb-test").toFile();
        storageDir.deleteOnExit();
        final Dataset dataset = TDBFactory.createDataset(storageDir.getAbsolutePath());
        dataset.begin(ReadWrite.WRITE);
        generateTestData(dataset);
        dataset.commit();
        TDB.sync(dataset);
        dataset.close();

        this.storage = new TDBStorage(createConfiguration(storageDir.getAbsolutePath()));
        storage.begin(ReadWrite.READ);
        assertTrue(storage.getDataset().getDefaultModel().contains(RESOURCE, RDF.type, createResource(TYPE_ONE)));
        storage.commit();
    }

    @Test
    public void initializeCreatesNewTDBStorageWhenTargetDirectoryDoesNotExist() throws Exception {
        this.storageDir = Files.createTempDirectory("tdb-test").toFile();
        storageDir.deleteOnExit();
        storageDir.delete();
        assertFalse(storageDir.exists());
        this.storage = new TDBStorage(createConfiguration(storageDir.getAbsolutePath()));
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
