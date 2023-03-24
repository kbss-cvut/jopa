package cz.cvut.kbss.ontodriver.rdf4j.loader;

import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.AxiomBuilder;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.query.BooleanQuery;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;

/**
 * Builds statement loaders for GraphDB repository access.
 */
public class GraphDBStatementLoaderFactory implements StatementLoaderFactory {

    /**
     * Property representing internal entity IDs in GraphDB.
     */
    static final String GRAPHDB_INTERNAL_ID_PROPERTY = "http://www.ontotext.com/owlim/entity#id";

    @Override
    public StatementLoader create(Connector connector, Resource subject, AxiomBuilder axiomBuilder) {
        return new GraphDBStatementLoader(connector, subject, axiomBuilder);
    }

    /**
     * Checks whether the underlying repository is in fact GraphDB.
     * <p>
     * It asks the repository if any subject has an internal GraphDB entity ID (represented by the {@link
     * #GRAPHDB_INTERNAL_ID_PROPERTY}). Such a triple does not normally show in the data, but is accessing in GraphDB.
     * If, for some reason, data stored in a non-GraphDB repository explicitly use these identifiers, this method will
     * return false positive result.
     *
     * @param repository RDF4J repository
     * @return {@code true} if repository is determined to be GraphDB, {@code false} otherwise
     */
    public static boolean isRepositoryGraphDB(Repository repository) throws Rdf4jDriverException {
        try {
            try (final RepositoryConnection connection = repository.getConnection()) {
                final ValueFactory vf = connection.getValueFactory();
                // Have to use a SPARQL query, because RDF4J API hasStatement call ended with an error
                // See https://graphdb.ontotext.com/documentation/standard/query-behaviour.html#how-to-access-internal-identifiers-for-entities
                final BooleanQuery query = connection.prepareBooleanQuery("ASK { ?x ?internalId ?y }");
                query.setBinding("internalId", vf.createIRI(GRAPHDB_INTERNAL_ID_PROPERTY));
                return query.evaluate();
            }
        } catch (RuntimeException e) {
            throw new Rdf4jDriverException(e);
        }
    }
}
