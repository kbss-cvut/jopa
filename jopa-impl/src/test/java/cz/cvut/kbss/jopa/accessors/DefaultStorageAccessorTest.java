/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.accessors;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Collections;

import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

public class DefaultStorageAccessorTest {

    private static final String DATA_SOURCE_CLASS = "cz.cvut.kbss.jopa.accessors.DataSourceStub";
    private static final String INVALID_DATA_SOURCE_CLASS = "cz.cvut.kbss.jopa.accessors.InvalidDataSource";

    @Mock
    private OntologyStorageProperties storageProperties;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(storageProperties.getDriver()).thenReturn(DATA_SOURCE_CLASS);
    }

    @Test
    public void createsDataSourceFromClassName() throws Exception {
        final DefaultStorageAccessor a = new DefaultStorageAccessor(storageProperties, Collections.<String, String>emptyMap());
        assertNotNull(a);
        assertTrue(a.isOpen());
        assertTrue(a.acquireConnection() instanceof DataSourceStub.ConnectionStub);
    }

    @Test(expected = DataSourceCreationException.class)
    public void throwsExceptionWhenDataSourceClassWithoutProperConstructorIsProvided() throws Exception {
        when(storageProperties.getDriver()).thenReturn(INVALID_DATA_SOURCE_CLASS);
        final DefaultStorageAccessor a = new DefaultStorageAccessor(storageProperties, Collections.<String, String>emptyMap());
        // This shouldn't be reached
        assertFalse(a.isOpen());
    }

    @Test
    public void testClose() throws Exception {
        final DefaultStorageAccessor a = new DefaultStorageAccessor(storageProperties, Collections.<String, String>emptyMap());
        assertTrue(a.isOpen());
        a.close();
        assertFalse(a.isOpen());
    }
}