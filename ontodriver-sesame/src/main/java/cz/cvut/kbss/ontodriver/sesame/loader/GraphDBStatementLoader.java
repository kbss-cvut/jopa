package cz.cvut.kbss.ontodriver.sesame.loader;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.util.AxiomBuilder;
import org.eclipse.rdf4j.model.Resource;

/**
 * Statement loader for GraphDB repositories.
 * <p>
 * It differs from the basic {@link StatementLoader} in the way inferred statements are loaded. This is because GraphDB
 * does not store inferred statements in the same context as the statements they are inferred from (as RDF4J does), but
 * instead has a special {@code implicit} context for them.
 */
public class GraphDBStatementLoader extends StatementLoader {

    public GraphDBStatementLoader(Connector connector, Resource subject, AxiomBuilder axiomBuilder) {
        super(connector, subject, axiomBuilder);
    }

    // TODO
}
