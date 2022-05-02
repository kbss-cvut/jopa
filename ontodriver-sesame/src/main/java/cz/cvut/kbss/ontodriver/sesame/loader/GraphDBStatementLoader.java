package cz.cvut.kbss.ontodriver.sesame.loader;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.sesame.util.AxiomBuilder;
import org.eclipse.rdf4j.model.Resource;

import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * Statement loader for GraphDB repositories.
 * <p>
 * It differs from the basic {@link StatementLoader} in the way inferred statements are loaded. This is because GraphDB
 * does not store inferred statements in the same context as the statements they are inferred from (as RDF4J does), but
 * instead has a special {@code implicit} context for them.
 */
public class GraphDBStatementLoader extends StatementLoader {

    /**
     * Repository pseudo-context used by GraphDB to store inferred statements.
     */
    static final URI GRAPHDB_IMPLICIT_CONTEXT = URI.create("http://www.ontotext.com/implicit");

    public GraphDBStatementLoader(Connector connector, Resource subject, AxiomBuilder axiomBuilder) {
        super(connector, subject, axiomBuilder);
    }

    @Override
    protected Set<URI> resolveContexts(AxiomDescriptor descriptor, Assertion a) {
        final Set<URI> contexts = new HashSet<>(super.resolveContexts(descriptor, a));
        if (includeInferred) {
            contexts.add(GRAPHDB_IMPLICIT_CONTEXT);
        }
        return contexts;
    }

    @Override
    public Collection<Axiom<?>> loadAxioms(Set<URI> contexts) throws SesameDriverException {
        if (includeInferred) {
            final Set<URI> contextsToUse = new HashSet<>(contexts);
            contextsToUse.add(GRAPHDB_IMPLICIT_CONTEXT);
            return super.loadAxioms(contextsToUse);
        }
        return super.loadAxioms(contexts);
    }
}
