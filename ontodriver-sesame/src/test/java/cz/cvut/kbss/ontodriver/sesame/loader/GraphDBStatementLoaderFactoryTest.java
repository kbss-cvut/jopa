package cz.cvut.kbss.ontodriver.sesame.loader;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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
        when(connection.hasStatement(any(), any(IRI.class), any(), anyBoolean())).thenReturn(true);

        assertTrue(GraphDBStatementLoaderFactory.isRepositoryGraphDB(connector));
        verify(connection).hasStatement(isNull(), eq(vf.createIRI(GraphDBStatementLoaderFactory.GRAPHDB_INTERNAL_ID_PROPERTY)), isNull(), eq(false));
    }
}