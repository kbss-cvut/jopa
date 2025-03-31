package cz.cvut.kbss.ontodriver.rdf4j.container;

import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ContainerValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.connector.RepoConnection;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.ValueConverter;
import cz.cvut.kbss.ontodriver.util.IdentifierUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;

/**
 * Handles RDF container access operations.
 */
public class ContainerHandler {

    private static final Logger LOG = LoggerFactory.getLogger(ContainerHandler.class);

    private static final String MEMBERSHIP_PROPERTY_URI_BASE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#_";
    private static final int MEMBERSHIP_PROPERTY_URI_BASE_LENGTH = MEMBERSHIP_PROPERTY_URI_BASE.length();

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
        Objects.requireNonNull(descriptor);
        final Set<IRI> contexts = contexts(descriptor);
        final Optional<Resource> container = findContainer(descriptor, contexts);
        if (container.isEmpty()) {
            return List.of();
        }
        final Collection<Statement> content = connector.findStatements(container.get(), null, null, false, contexts);
        return (List) content.stream()
                             .filter(s -> s.getPredicate().stringValue().startsWith(MEMBERSHIP_PROPERTY_URI_BASE))
                             .sorted(ContainerHandler::statementComparator)
                             .map(s -> ValueConverter.fromRdf4jValue(descriptor.getProperty(), s.getObject()))
                             .filter(Optional::isPresent)
                             .map(o -> new AxiomImpl<>(descriptor.getOwner(), descriptor.getProperty(), new cz.cvut.kbss.ontodriver.model.Value<>(o.get())))
                             .toList();
    }

    private Optional<Resource> findContainer(ContainerDescriptor descriptor,
                                             Set<IRI> contexts) throws Rdf4jDriverException {
        final IRI owner = vf.createIRI(descriptor.getOwner().getIdentifier().toString());
        final IRI property = vf.createIRI(descriptor.getProperty().getIdentifier().toString());
        final Collection<Statement> statements = connector.findStatements(owner, property, null, false, contexts);
        if (statements.isEmpty()) {
            return Optional.empty();
        }
        if (statements.size() > 1) {
            throw new IntegrityConstraintViolatedException("Expected a single value of property <" + property + ">, but got multiple.");
        }
        final Value containerValue = statements.iterator().next().getObject();
        assert containerValue.isResource();
        return Optional.of((Resource) containerValue);
    }

    private static int statementComparator(Statement s1, Statement s2) {
        final String p1 = s1.getPredicate().toString();
        final String p2 = s2.getPredicate().toString();
        try {
            final int p1Number = Integer.parseInt(p1.substring(MEMBERSHIP_PROPERTY_URI_BASE_LENGTH));
            final int p2Number = Integer.parseInt(p2.substring(MEMBERSHIP_PROPERTY_URI_BASE_LENGTH));
            return p1Number - p2Number;
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Unable to determine container membership property number.", e);
        }
    }

    private Set<IRI> contexts(ContainerDescriptor descriptor) {
        return descriptor.getContext() != null ? Set.of(vf.createIRI(descriptor.getContext().toString())) : Set.of();
    }

    /**
     * Creates a new container and fills it with the specified values.
     *
     * @param descriptor Container value descriptor
     * @param <T>        Type of container values
     * @throws Rdf4jDriverException If an error accessing the container occurs
     */
    public <T> void persistContainer(ContainerValueDescriptor<T> descriptor) throws Rdf4jDriverException {
        Objects.requireNonNull(descriptor);
        if (descriptor.getValues().isEmpty()) {
            LOG.trace("Container is empty, nothing to persist.");
            return;
        }
        final IRI containerIri = generateContainerIri(descriptor);
        final IRI contextIri = descriptor.getContext() != null ? vf.createIRI(descriptor.getContext()
                                                                                        .toString()) : null;
        final List<Statement> toPersist = new ArrayList<>();
        toPersist.add(vf.createStatement(toRdf4jIri(descriptor.getOwner()
                                                              .getIdentifier()), toRdf4jIri(descriptor.getProperty()
                                                                                                      .getIdentifier()), containerIri, contextIri));
        toPersist.add(vf.createStatement(containerIri, RDF.TYPE, toRdf4jIri(descriptor.getType()), contextIri));
        toPersist.addAll(generateContainerContent(descriptor, containerIri, contextIri));
        connector.addStatements(toPersist);
    }

    private IRI toRdf4jIri(URI uri) {
        return vf.createIRI(uri.toString());
    }

    private IRI generateContainerIri(ContainerValueDescriptor<?> descriptor) throws Rdf4jDriverException {
        IRI iri;
        do {
            iri = vf.createIRI(descriptor.getOwner().getIdentifier().toString() + descriptor.getType().getFragment()
                                                                                            .toLowerCase() + "_" + IdentifierUtils.randomInt());
            LOG.trace("Generated container IRI: <{}>", iri);
        } while (connector.containsStatement(iri, null, null, false, Set.of()));
        return iri;
    }

    private <T> List<Statement> generateContainerContent(ContainerValueDescriptor<T> descriptor, Resource container,
                                                         IRI contextIri) throws Rdf4jDriverException {
        final List<Statement> result = new ArrayList<>(descriptor.getValues().size());
        final ValueConverter valueConverter = new ValueConverter(vf);
        for (int i = 0; i < descriptor.getValues().size(); i++) {
            final T value = descriptor.getValues().get(i);
            result.add(vf.createStatement(container, vf.createIRI(MEMBERSHIP_PROPERTY_URI_BASE + (i + 1)), valueConverter.toRdf4jValue(descriptor.getProperty(), value), contextIri));
        }
        return result;
    }

    /**
     * Updates the content of an existing container corresponding to the specified descriptor.
     * <p>
     * If the descriptor has no value, the container is removed completely.
     *
     * @param descriptor Descriptor with new container values
     * @param <T>        Value type
     * @throws Rdf4jDriverException If an error accessing the container occurs
     */
    public <T> void updateContainer(ContainerValueDescriptor<T> descriptor) throws Rdf4jDriverException {
        Objects.requireNonNull(descriptor);
        final IRI context = descriptor.getContext() != null ? vf.createIRI(descriptor.getContext().toString()) : null;
        final Set<IRI> contexts = context != null ? Set.of(context) : Set.of();
        final Optional<Resource> container = findContainer(descriptor, contexts);
        if (container.isEmpty()) {
            persistContainer(descriptor);
            return;
        }
        if (descriptor.getValues().isEmpty()) {
            deleteContainer(descriptor.getOwner(), descriptor.getProperty(), container.get(), contexts);
            return;
        }
        // The current implementation is simple - just clear the container and add its new content
        // If performance becomes a concern, we can try merging the existing content with the updated one
        // (paying attention to the type of the container, as it has effect on whether the order matters or not)
        clearContainer(container.get(), contexts);
        connector.addStatements(generateContainerContent(descriptor, container.get(), context));
    }

    private void clearContainer(Resource container, Set<IRI> contexts) throws Rdf4jDriverException {
        final Collection<Statement> content = connector.findStatements(container, null, null, false, contexts);
        connector.removeStatements(content.stream().filter(Predicate.not(s -> RDF.TYPE.equals(s.getPredicate())))
                                          .toList());
    }

    private void deleteContainer(NamedResource owner, Assertion property, Resource container,
                                 Set<IRI> contexts) throws Rdf4jDriverException {
        final List<Statement> toRemove = new ArrayList<>(connector.findStatements(container, null, null, false, contexts));
        toRemove.addAll(connector.findStatements(toRdf4jIri(owner.getIdentifier()), toRdf4jIri(property.getIdentifier()), container, false, contexts));
        connector.removeStatements(toRemove);
    }
}
