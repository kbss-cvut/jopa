package cz.cvut.kbss.ontodriver.rdf4j.loader;

import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.query.BooleanQuery;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class GraphDBStatementLoaderFactoryTest {

    @Mock
    private Repository repository;

    @Mock
    private RepositoryConnection connection;

    @Mock
    private Connector connector;

    @Test
    void isRepositoryGraphDBChecksForPresenceOfInternalGraphDBEntityIds() throws Exception {
        when(repository.getConnection()).thenReturn(connection);
        final ValueFactory vf = SimpleValueFactory.getInstance();
        when(connection.getValueFactory()).thenReturn(vf);
        when(connector.unwrap(Repository.class)).thenReturn(repository);
        final BooleanQuery query = mock(BooleanQuery.class);
        when(connection.prepareBooleanQuery(anyString())).thenReturn(query);
        when(query.evaluate()).thenReturn(true);

        assertTrue(GraphDBStatementLoaderFactory.isRepositoryGraphDB(connector));
        verify(query).setBinding(anyString(), eq(vf.createIRI(GraphDBStatementLoaderFactory.GRAPHDB_INTERNAL_ID_PROPERTY)));
    }
}