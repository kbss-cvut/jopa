package cz.cvut.kbss.ontodriver.rdf4j.container;

import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.rdf4j.connector.RepoConnection;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.ValueConverter;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * Handles RDF container access operations.
 */
public class ContainerHandler {

    private static final int MEMBERSHIP_PROPERTY_NUMBER_PREFIX = "http://www.w3.org/1999/02/22-rdf-syntax-ns#_".length();

    private final RepoConnection connector;
    private final ValueFactory vf;

    public ContainerHandler(RepoConnection connector, ValueFactory vf) {
        this.connector = connector;
        this.vf = vf;
    }

    /**
     * Loads the content of a container corresponding to the specified description.
     * <p>
     * The container values are returned as a list of axioms where the owner of the container points to the individual
     * values.
     *
     * @param descriptor Container description
     * @return List of axioms representing the container's content
     * @throws Rdf4jDriverException If an error accessing the container occurs
     */
    public List<Axiom<?>> loadContainer(ContainerDescriptor descriptor) throws Rdf4jDriverException {
        final IRI owner = vf.createIRI(descriptor.getOwner().getIdentifier().toString());
        final IRI property = vf.createIRI(descriptor.getProperty().getIdentifier().toString());
        final boolean includeInferred = descriptor.getProperty().isInferred();
        final Set<IRI> contexts = contexts(descriptor);
        final Collection<Statement> statements = connector.findStatements(owner, property, null, includeInferred, contexts);
        if (statements.isEmpty()) {
            return List.of();
        }
        if (statements.size() > 1) {
            throw new IntegrityConstraintViolatedException("Expected a single value of property <" + property + ">, but got multiple.");
        }
        final Value containerValue = statements.iterator().next().getObject();
        assert containerValue.isResource();
        final Resource container = (Resource) containerValue;
        final Collection<Statement> content = connector.findStatements(container, null, null, includeInferred, contexts);
        return (List) content.stream()
                             .sorted(ContainerHandler::statementComparator)
                             .map(s -> ValueConverter.fromRdf4jValue(descriptor.getProperty(), s.getObject()))
                             .filter(Optional::isPresent)
                             .map(o -> new AxiomImpl<>(descriptor.getOwner(), descriptor.getProperty(), new cz.cvut.kbss.ontodriver.model.Value<>(o.get())))
                             .toList();
    }

    private static int statementComparator(Statement s1, Statement s2) {
        final String p1 = s1.getPredicate().toString();
        final String p2 = s2.getPredicate().toString();
        try {
            final int p1Number = Integer.parseInt(p1.substring(MEMBERSHIP_PROPERTY_NUMBER_PREFIX));
            final int p2Number = Integer.parseInt(p2.substring(MEMBERSHIP_PROPERTY_NUMBER_PREFIX));
            return p1Number - p2Number;
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Unable to determine container membership property number.", e);
        }
    }

    private Set<IRI> contexts(ContainerDescriptor descriptor) {
        return descriptor.getContext() != null ? Set.of(vf.createIRI(descriptor.getContext().toString())) : Set.of();
    }
}
