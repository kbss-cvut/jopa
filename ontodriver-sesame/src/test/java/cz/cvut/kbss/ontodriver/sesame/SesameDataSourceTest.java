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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.sesame.environment.TestUtils;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.sail.SailRepository;
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class SesameDataSourceTest {

    @Mock
    private SesameDriver driverMock;

    private SesameDataSource dataSource;

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.dataSource = new SesameDataSource();
        TestUtils.setMock("driver", dataSource, driverMock);
    }

    @Test
    public void testSesameDataSourceSetNullStorageProperties() {
        final SesameDataSource ds = new SesameDataSource();
        assertThrows(NullPointerException.class, () -> ds.setStorageProperties(null));
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

    @Test
    public void testGetConnectionWithoutInitialization() {
        assertThrows(IllegalStateException.class, () -> dataSource.getConnection());
    }

    @Test
    public void testGetConnectionOnClosed() throws Exception {
        dataSource.close();
        assertFalse(dataSource.isOpen());
        try {
            assertThrows(IllegalStateException.class, () -> dataSource.getConnection());
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
        final SesameDriverException ex = assertThrows(SesameDriverException.class,
                () -> dataSource.setRepository(null));
        assertThat(ex.getCause(), instanceOf(IllegalStateException.class));
    }
}
