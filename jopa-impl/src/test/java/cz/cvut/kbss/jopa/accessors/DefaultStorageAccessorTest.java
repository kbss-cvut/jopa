/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.accessors;

import cz.cvut.kbss.jopa.exception.DataSourceCreationException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DefaultStorageAccessorTest {

    private static final String DATA_SOURCE_CLASS = DataSourceStub.class.getName();
    private static final String INVALID_DATA_SOURCE_CLASS = InvalidDataSource.class.getName();

    @Test
    void createsDataSourceFromClassName() {
        final DefaultStorageAccessor a =
                new DefaultStorageAccessor(storageProperties(DATA_SOURCE_CLASS), Collections.emptyMap());
        assertNotNull(a);
        assertTrue(a.isOpen());
        assertInstanceOf(DataSourceStub.ConnectionStub.class, a.acquireConnection());
    }

    private OntologyStorageProperties storageProperties(String driverClass) {
        return OntologyStorageProperties.physicalUri(URI.create("file:/tmp/file")).driver(driverClass).build();
    }

    @Test
    void throwsExceptionWhenDataSourceClassWithoutProperConstructorIsProvided() {
        final DataSourceCreationException ex = assertThrows(DataSourceCreationException.class,
                () -> new DefaultStorageAccessor(storageProperties(INVALID_DATA_SOURCE_CLASS), Collections.emptyMap()));
        assertEquals("Unable to create instance of OntoDriver data source " + INVALID_DATA_SOURCE_CLASS,
                ex.getMessage());
    }

    @Test
    void throwsExceptionWhenUnknownClassIsSpecifiedAsDataSource() {
        final String unknownClass = "cz.cvut.kbss.jopa.UnknownDataSource";
        final DataSourceCreationException ex = assertThrows(DataSourceCreationException.class,
                                                            () -> new DefaultStorageAccessor(storageProperties(unknownClass), Collections.emptyMap()));
        assertEquals("Unable to find OntoDriver data source class " + unknownClass, ex.getMessage());
    }

    @Test
    void testClose() {
        final DefaultStorageAccessor a = new DefaultStorageAccessor(storageProperties(DATA_SOURCE_CLASS),
                Collections.emptyMap());
        assertTrue(a.isOpen());
        a.close();
        assertFalse(a.isOpen());
    }

    @Test
    void closingAccessorMultipleTimesDoesNothing() {
        final DefaultStorageAccessor a = new DefaultStorageAccessor(storageProperties(DATA_SOURCE_CLASS),
                Collections.emptyMap());
        assertTrue(a.isOpen());
        a.close();
        assertFalse(a.isOpen());
        a.close();
        assertFalse(a.isOpen());
    }

    @Test
    void gettingExceptionWhenTryingToAcquireConnectionThrowsStorageAccessException() throws Exception {
        final DefaultStorageAccessor a = new DefaultStorageAccessor(storageProperties(DATA_SOURCE_CLASS),
                Collections.emptyMap());
        final DataSourceStub ds = getDataSourceStub(a);
        ds.throwExceptionOnGetConnection();
        final StorageAccessException ex = assertThrows(StorageAccessException.class, a::acquireConnection);
        assertEquals("Unable to acquire storage connection.", ex.getMessage());
    }

    private DataSourceStub getDataSourceStub(DefaultStorageAccessor a) throws NoSuchFieldException,
            IllegalAccessException {
        final Field dsField = DefaultStorageAccessor.class.getDeclaredField("dataSource");
        dsField.setAccessible(true);
        return (DataSourceStub) dsField.get(a);
    }

    @Test
    void gettingExceptionWhenTryingToCloseDataSourceThrowsStorageAccessExceptionAndClosesAccessor() throws
            Exception {
        final DefaultStorageAccessor a = new DefaultStorageAccessor(storageProperties(DATA_SOURCE_CLASS),
                Collections.emptyMap());
        final DataSourceStub ds = getDataSourceStub(a);
        ds.throwExceptionOnClose();
        try {
            final StorageAccessException ex = assertThrows(StorageAccessException.class, a::close);
            assertEquals("Error when closing the data source.", ex.getMessage());
        } finally {
            assertFalse(a.isOpen());
        }
    }

    @Test
    void unwrapReturnsMatchingDataSourceInstance() {
        final DefaultStorageAccessor a =
                new DefaultStorageAccessor(storageProperties(DATA_SOURCE_CLASS), Collections.emptyMap());
        final DataSourceStub ds = a.unwrap(DataSourceStub.class);
        assertNotNull(ds);
    }

    @Test
    void unwrapThrowsExceptionWhenNoMatchingInstanceIsFound() {
        final DefaultStorageAccessor a =
                new DefaultStorageAccessor(storageProperties(DATA_SOURCE_CLASS), Collections.emptyMap());
        final OWLPersistenceException ex = assertThrows(OWLPersistenceException.class, () -> a.unwrap(String.class));
        assertEquals("Instance of class " + String.class + " not found.", ex.getMessage());
    }
}
