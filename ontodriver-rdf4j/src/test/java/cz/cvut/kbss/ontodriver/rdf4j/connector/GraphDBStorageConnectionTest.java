package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.common.transaction.IsolationLevel;
import org.eclipse.rdf4j.common.transaction.IsolationLevels;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class GraphDBStorageConnectionTest {

    @ParameterizedTest
    @CsvSource({
            "SNAPSHOT_READ",
            "SNAPSHOT",
            "SERIALIZABLE"
    })
    void constructorThrowsExceptionWhenIsolationLevelIsGreaterThanReadCommitted(String level) {
        final StorageConnector connector = mock(StorageConnector.class);
        final IsolationLevel isolationLevel = IsolationLevels.valueOf(level);
        assertThrows(Rdf4jDriverException.class, () -> new GraphDBStorageConnection(connector, isolationLevel));
    }

    @Test
    void constructorAllowsNullIsolationLevel() {
        final StorageConnector connector = mock(StorageConnector.class);
        assertDoesNotThrow(() -> new GraphDBStorageConnection(connector, null));
    }

    @Test
    void beginAcquiresConnectionAndStartsTransaction() throws Exception {
        final StorageConnector connector = mock(StorageConnector.class);
        final RepositoryConnection connection = mock(RepositoryConnection.class);
        when(connector.acquireConnection()).thenReturn(connection);
        final GraphDBStorageConnection sut = new GraphDBStorageConnection(connector, null);
        sut.begin();
        verify(connector).acquireConnection();
        verify(connection).begin((IsolationLevel) null);
    }

    @Test
    void beginAcquiresConnectionButDoesNotStartTransactionWhenReadOnlyIsTrue() throws Exception {
        final StorageConnector connector = mock(StorageConnector.class);
        final RepositoryConnection connection = mock(RepositoryConnection.class);
        when(connector.acquireConnection()).thenReturn(connection);
        final GraphDBStorageConnection sut = new GraphDBStorageConnection(connector, null);
        sut.setReadOnly(true);
        sut.begin();
        verify(connector).acquireConnection();
        verify(connection, never()).begin((IsolationLevel) null);
    }
}
