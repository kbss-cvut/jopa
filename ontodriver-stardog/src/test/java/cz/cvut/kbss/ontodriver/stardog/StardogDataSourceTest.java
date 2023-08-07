package cz.cvut.kbss.ontodriver.stardog;

import cz.cvut.kbss.ontodriver.Connection;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class StardogDataSourceTest {

    @Mock
    private StardogDriver driverMock;

    @InjectMocks
    private StardogDataSource dataSource;

    @Test
    public void setStoragePropertiesWithNullArgumentThrowsNullPointerException() {
        final StardogDataSource ds = new StardogDataSource();
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
}
