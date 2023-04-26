package cz.cvut.kbss.ontodriver.rdf4j.loader;

import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.util.AxiomBuilder;
import org.eclipse.rdf4j.model.Resource;

/**
 * Builds statement loaders for GraphDB repository access.
 */
public class GraphDBStatementLoaderFactory implements StatementLoaderFactory {

    @Override
    public StatementLoader create(Connector connector, Resource subject, AxiomBuilder axiomBuilder) {
        return new GraphDBStatementLoader(connector, subject, axiomBuilder);
    }
}
