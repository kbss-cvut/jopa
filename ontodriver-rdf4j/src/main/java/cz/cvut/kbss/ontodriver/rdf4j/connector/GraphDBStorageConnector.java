package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.rdf4j.connector.init.RepositoryConnectorInitializer;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;

import java.util.Collection;

public class GraphDBStorageConnector extends StorageConnector {

    public GraphDBStorageConnector(RepositoryConnectorInitializer repoInitializer) {
        super(repoInitializer);
    }

    @Override
    public boolean isInferred(Statement statement, Collection<IRI> contexts) throws Rdf4jDriverException {
        try (final RepositoryConnection conn = acquireConnection()) {
            final IRI[] ctxArr = contexts.toArray(new IRI[0]);
            // Inferred statements are in the implicit graph in GraphDB. This graph is not accessible via the RDF4J API
            return conn.hasStatement(statement, true) && !conn.hasStatement(statement, false, ctxArr);
        } catch (RepositoryException e) {
            throw new Rdf4jDriverException(e);
        }
    }
}
