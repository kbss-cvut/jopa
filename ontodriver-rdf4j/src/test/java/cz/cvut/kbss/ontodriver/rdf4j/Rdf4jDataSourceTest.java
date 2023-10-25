/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.sail.SailRepository;
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class Rdf4jDataSourceTest {

    @Mock
    private Rdf4jDriver driverMock;

    @InjectMocks
    private Rdf4jDataSource dataSource;

    @Test
    public void testRdf4jDataSourceSetNullStorageProperties() {
        final Rdf4jDataSource ds = new Rdf4jDataSource();
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
    public void setRepositoryWrapsIllegalStateExceptionIntoRdf4jDriverException() throws Exception {
        Field connected = dataSource.getClass().getDeclaredField("connected");
        connected.setAccessible(true);
        connected.set(dataSource, true);
        doThrow(new IllegalStateException()).when(driverMock).setRepository(any());
        final Rdf4jDriverException ex = assertThrows(Rdf4jDriverException.class,
                                                     () -> dataSource.setRepository(null));
        assertThat(ex.getCause(), instanceOf(IllegalStateException.class));
    }
}
