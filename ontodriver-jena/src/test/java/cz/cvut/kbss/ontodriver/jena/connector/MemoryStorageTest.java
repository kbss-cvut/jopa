/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.sparql.core.DatasetImpl;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class MemoryStorageTest extends StorageTestUtil {

    @Test
    public void initializationCreatesTransactionalInMemoryDataset() {

        final LocalStorage storage = new MemoryStorage(StorageTestUtil.createConfiguration("urn:test"));
        final Dataset dataset = storage.getDataset();
        assertNotNull(dataset);
        assertTrue(dataset.supportsTransactions());
    }

    @Test
    public void setDatasetReplacesStorageDataset() {
        final LocalStorage storage = new MemoryStorage(StorageTestUtil.createConfiguration("urn:test"));

        final Dataset newDataset = DatasetFactory.createTxnMem();
        generateTestData(newDataset);
        storage.setDataset(newDataset);
        assertEquals(newDataset, storage.getDataset());
    }

    @Test
    public void setDatasetThrowsIllegalArgumentWhenDatasetDoesNotSupportTransactions() {
        final LocalStorage storage = new MemoryStorage(StorageTestUtil.createConfiguration("urn:test"));

        final Dataset newDataset = new NonTransactionDataset(ModelFactory.createDefaultModel());
        assertFalse(newDataset.supportsTransactions());
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> storage.setDataset(newDataset));
        assertEquals("The provided dataset does not support transactions.", ex.getMessage());
    }

    private static final class NonTransactionDataset extends DatasetImpl {

        private NonTransactionDataset(Model model) {
            super(model);
        }

        @Override
        public boolean supportsTransactions() {
            return false;
        }
    }

    @Test
    public void setDatasetThrowsIllegalStateWhenDatasetIsInTransaction() {
        final LocalStorage storage = new MemoryStorage(StorageTestUtil.createConfiguration("urn:test"));
        storage.begin(ReadWrite.WRITE);

        final IllegalStateException ex = assertThrows(IllegalStateException.class,
                () -> storage.setDataset(DatasetFactory.create()));
        assertEquals("Cannot replace dataset when it is in transaction.", ex.getMessage());
    }
}
