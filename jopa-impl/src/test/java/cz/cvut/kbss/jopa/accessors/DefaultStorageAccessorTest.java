/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.accessors;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;

import static org.junit.Assert.*;

public class DefaultStorageAccessorTest {

    private static final String DATA_SOURCE_CLASS = DataSourceStub.class.getName();
    private static final String INVALID_DATA_SOURCE_CLASS = InvalidDataSource.class.getName();

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void createsDataSourceFromClassName() {
        final DefaultStorageAccessor a =
                new DefaultStorageAccessor(storageProperties(DATA_SOURCE_CLASS), Collections.emptyMap());
        assertNotNull(a);
        assertTrue(a.isOpen());
        assertTrue(a.acquireConnection() instanceof DataSourceStub.ConnectionStub);
    }

    private OntologyStorageProperties storageProperties(String driverClass) {
        return OntologyStorageProperties.physicalUri(URI.create("file:/tmp/file")).driver(driverClass).build();
    }

    @Test
    public void throwsExceptionWhenDataSourceClassWithoutProperConstructorIsProvided() {
        thrown.expect(DataSourceCreationException.class);
        thrown.expectMessage("Unable to create instance of OntoDriver data source " + INVALID_DATA_SOURCE_CLASS);
        new DefaultStorageAccessor(storageProperties(INVALID_DATA_SOURCE_CLASS), Collections.emptyMap());
    }

    @Test
    public void throwsExceptionWhenUnknownClassIsSpecifiedAsDataSource() {
        final String unknownClass = "cz.cvut.kbss.jopa.UnknownDataSource";
        thrown.expect(DataSourceCreationException.class);
        thrown.expectMessage("Unable to find OntoDriver data source class " + unknownClass);
        new DefaultStorageAccessor(storageProperties(unknownClass), Collections.emptyMap());
    }

    @Test
    public void testClose() {
        final DefaultStorageAccessor a = new DefaultStorageAccessor(storageProperties(DATA_SOURCE_CLASS),
                Collections.emptyMap());
        assertTrue(a.isOpen());
        a.close();
        assertFalse(a.isOpen());
    }

    @Test
    public void closingAccessorMultipleTimesDoesNothing() {
        final DefaultStorageAccessor a = new DefaultStorageAccessor(storageProperties(DATA_SOURCE_CLASS),
                Collections.emptyMap());
        assertTrue(a.isOpen());
        a.close();
        assertFalse(a.isOpen());
        a.close();
        assertFalse(a.isOpen());
    }

    @Test
    public void gettingExceptionWhenTryingToAcquireConnectionThrowsStorageAccessException() throws Exception {
        thrown.expect(StorageAccessException.class);
        thrown.expectMessage("Unable to acquire storage connection.");
        final DefaultStorageAccessor a = new DefaultStorageAccessor(storageProperties(DATA_SOURCE_CLASS),
                Collections.emptyMap());
        final DataSourceStub ds = getDataSourceStub(a);
        ds.throwExceptionOnGetConnection();
        a.acquireConnection();
    }

    private DataSourceStub getDataSourceStub(DefaultStorageAccessor a) throws NoSuchFieldException,
            IllegalAccessException {
        final Field dsField = DefaultStorageAccessor.class.getDeclaredField("dataSource");
        dsField.setAccessible(true);
        return (DataSourceStub) dsField.get(a);
    }

    @Test
    public void gettingExceptionWhenTryingToCloseDataSourceThrowsStorageAccessExceptionAndClosesAccessor() throws
            Exception {
        thrown.expect(StorageAccessException.class);
        thrown.expectMessage("Error when closing the data source.");
        final DefaultStorageAccessor a = new DefaultStorageAccessor(storageProperties(DATA_SOURCE_CLASS),
                Collections.emptyMap());
        final DataSourceStub ds = getDataSourceStub(a);
        ds.throwExceptionOnClose();
        try {
            a.close();
        } finally {
            assertFalse(a.isOpen());
        }
    }

    @Test
    public void unwrapReturnsMatchingDataSourceInstance() {
        final DefaultStorageAccessor a =
                new DefaultStorageAccessor(storageProperties(DATA_SOURCE_CLASS), Collections.emptyMap());
        final DataSourceStub ds = a.unwrap(DataSourceStub.class);
        assertNotNull(ds);
    }

    @Test
    public void unwrapThrowsExceptionWhenNoMatchingInstanceIsFound() {
        thrown.expect(OWLPersistenceException.class);
        thrown.expectMessage("Instance of class " + String.class + " not found.");
        final DefaultStorageAccessor a =
                new DefaultStorageAccessor(storageProperties(DATA_SOURCE_CLASS), Collections.emptyMap());
        a.unwrap(String.class);
    }
}