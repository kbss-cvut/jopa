package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.Wrapper;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.repository.RepositoryConnection;

/**
 * Provides {@link RepositoryConnection}s and other RDF4J-specific objects.
 */
public interface Rdf4jConnectionProvider extends Wrapper {

    RepositoryConnection acquireConnection() throws Rdf4jDriverException;

    ValueFactory getValueFactory();
}
