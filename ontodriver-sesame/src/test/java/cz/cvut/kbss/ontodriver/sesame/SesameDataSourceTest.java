/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.sesame.environment.TestUtils;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.sail.SailRepository;
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.hamcrest.CoreMatchers;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;

public class SesameDataSourceTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private SesameDriver driverMock;

    private SesameDataSource dataSource;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.dataSource = new SesameDataSource();
        TestUtils.setMock("driver", dataSource, driverMock);
    }

    @Test(expected = NullPointerException.class)
    public void testSesameDataSourceSetNullStorageProperties() {
        final SesameDataSource ds = new SesameDataSource();
        ds.setStorageProperties(null);
        fail("This line should not have been reached.");
        assert ds == null;
    }

    @Test
    public void testClose() throws Exception {
        assertTrue(dataSource.isOpen());
        dataSource.close();
        assertFalse(dataSource.isOpen());
        dataSource.close();
        assertFalse(dataSource.isOpen());
    }

    @Test
    public void testGetConnection() throws Exception {
        Field connected = dataSource.getClass().getDeclaredField("connected");
        connected.setAccessible(true);
        connected.set(dataSource, true);
        final Connection res = dataSource.getConnection();
        verify(driverMock).acquireConnection();
        assertNull(res);
    }

    @Test(expected = IllegalStateException.class)
    public void testGetConnectionWithoutInitialization() throws Exception {
        final Connection res = dataSource.getConnection();
        // This shouldn't be reached
        assert res == null;
    }

    @Test(expected = IllegalStateException.class)
    public void testGetConnectionOnClosed() throws Exception {
        dataSource.close();
        assertFalse(dataSource.isOpen());
        try {
            dataSource.getConnection();
        } finally {
            verify(driverMock, never()).acquireConnection();
        }
    }

    @Test
    public void setRepositorySetsUnderlyingRepository() throws Exception {
        Field connected = dataSource.getClass().getDeclaredField("connected");
        connected.setAccessible(true);
        connected.set(dataSource, true);
        final Repository repo = new SailRepository(new MemoryStore());
        repo.initialize();
        try {
            dataSource.setRepository(repo);
            verify(driverMock).setRepository(repo);
        } finally {
            repo.shutDown();
        }
    }

    @Test
    public void setRepositoryWrapsIllegalStateExceptionIntoSesameDriverException() throws Exception {
        Field connected = dataSource.getClass().getDeclaredField("connected");
        connected.setAccessible(true);
        connected.set(dataSource, true);
        doThrow(new IllegalStateException()).when(driverMock).setRepository(any());
        thrown.expect(SesameDriverException.class);
        thrown.expectCause(CoreMatchers.isA(IllegalStateException.class));
        dataSource.setRepository(null);
    }
}
