package cz.cvut.kbss.ontodriver.jena.container;

import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ContainerValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.util.JenaUtils;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.util.IdentifierUtils;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.vocabulary.RDF;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;

public class ContainerHandler {

    private static final Logger LOG = LoggerFactory.getLogger(ContainerHandler.class);

    private static final String MEMBERSHIP_PROPERTY_URI_BASE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#_";
    private static final int MEMBERSHIP_PROPERTY_URI_BASE_LENGTH = MEMBERSHIP_PROPERTY_URI_BASE.length();

    private final StorageConnector connector;

    public ContainerHandler(StorageConnector connector) {
        this.connector = connector;
    }

    public List<Axiom<?>> readContainer(ContainerDescriptor descriptor) throws JenaDriverException {
        Objects.requireNonNull(descriptor);
        final Optional<Resource> container = findContainer(descriptor);
        if (container.isEmpty()) {
            return List.of();
        }
        return readContainerContent(container.get(), descriptor);
    }

    private Optional<Resource> findContainer(ContainerDescriptor descriptor) throws JenaDriverException {
        final Resource subject = ResourceFactory.createResource(descriptor.getOwner().getIdentifier().toString());
        final Property property = ResourceFactory.createProperty(descriptor.getProperty().getIdentifier().toString());
        final Collection<Statement> containerStatements = connector.find(subject, property, null, contexts(descriptor));
        if (containerStatements.isEmpty()) {
            return Optional.empty();
        }
        if (containerStatements.size() > 1) {
            throw new IntegrityConstraintViolatedException("Expected a single value of property <" + property + ">, but got multiple.");
        }
        final RDFNode container = containerStatements.iterator().next().getObject();
        if (!container.isResource()) {
            throw new JenaDriverException("Expected the value of property <" + property + "> to be a resource, but got literal.");
        }
        return Optional.of(containerStatements.iterator().next().getObject().asResource());
    }

    private static List<String> contexts(ContainerDescriptor descriptor) {
        return descriptor.getContext() != null ? List.of(descriptor.getContext().toString()) : List.of();
    }

    private List<Axiom<?>> readContainerContent(Resource container, ContainerDescriptor descriptor) {
        return findContainerMembers(container, contexts(descriptor))
                .sorted(ContainerHandler::statementComparator)
                .map(s -> {
                    final RDFNode element = s.getObject();
                    if (element.isResource()) {
                        return new AxiomImpl<>(descriptor.getOwner(), descriptor.getProperty(), new Value<>(NamedResource.create(element.asResource()
                                                                                                                                        .getURI())));
                    } else {
                        return (Axiom<?>) new AxiomImpl<>(descriptor.getOwner(), descriptor.getProperty(), new Value<>(JenaUtils.literalToValue(element.asLiteral())));
                    }
                }).toList();

    }

    private Stream<Statement> findContainerMembers(Resource container, List<String> contexts) {
        return connector.find(container, null, null, contexts).stream()
                        .filter(s -> s.getPredicate().getURI().startsWith(MEMBERSHIP_PROPERTY_URI_BASE));
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

    /**
     * Creates a new container and fills it with the specified values.
     *
     * @param descriptor Container value descriptor
     * @param <T>        Type of container values
     */
    public <T> void persistContainer(ContainerValueDescriptor<T> descriptor) {
        Objects.requireNonNull(descriptor);
        if (descriptor.getValues().isEmpty()) {
            return;
        }
        final Resource container = generateContainerIri(descriptor);
        final List<Statement> toAdd = new ArrayList<>(descriptor.getValues().size() + 2);
        toAdd.add(ResourceFactory.createStatement(ResourceFactory.createResource(descriptor.getOwner().getIdentifier()
                                                                                           .toString()),
                ResourceFactory.createProperty(descriptor.getProperty().getIdentifier().toString()),
                container));
        toAdd.add(ResourceFactory.createStatement(container, RDF.type, ResourceFactory.createResource(descriptor.getType()
                                                                                                                .toString())));
        toAdd.addAll(generateContainerContent(descriptor, container));
        connector.add(toAdd, descriptor.getContext() != null ? descriptor.getContext().toString() : null);
    }

    private <T> Resource generateContainerIri(ContainerValueDescriptor<T> descriptor) {
        Resource container;
        do {
            container = ResourceFactory.createResource(descriptor.getOwner().getIdentifier()
                                                                 .toString() + descriptor.getType().getFragment()
                                                                                         .toLowerCase() + "_" + IdentifierUtils.randomInt());
            LOG.trace("Generated container IRI: <{}>", container);
        } while (connector.contains(container, null, null, contexts(descriptor)));
        return container;
    }

    private List<Statement> generateContainerContent(ContainerValueDescriptor<?> descriptor, Resource container) {
        final List<Statement> result = new ArrayList<>(descriptor.getValues().size());
        for (int i = 0; i < descriptor.getValues().size(); i++) {
            result.add(ResourceFactory.createStatement(container,
                    ResourceFactory.createProperty(MEMBERSHIP_PROPERTY_URI_BASE + (i + 1)),
                    JenaUtils.toRdfNode(descriptor.getProperty(), descriptor.getValues().get(i))));
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
     * @throws JenaDriverException If an error accessing the container occurs
     */
    public <T> void updateContainer(ContainerValueDescriptor<T> descriptor) throws JenaDriverException {
        Objects.requireNonNull(descriptor);
        final Optional<Resource> container = findContainer(descriptor);
        if (container.isEmpty()) {
            persistContainer(descriptor);
            return;
        }
        final List<Statement> toRemove = new ArrayList<>(findContainerMembers(container.get(), contexts(descriptor)).toList());
        final String context = descriptor.getContext() != null ? descriptor.getContext().toString() : null;
        if (descriptor.getValues().isEmpty()) {
            toRemove.addAll(removeContainer(descriptor, container.get()));
            connector.remove(toRemove, context);
            return;
        }
        connector.remove(toRemove, context);
        connector.add(generateContainerContent(descriptor, container.get()), context);
    }

    private List<Statement> removeContainer(ContainerDescriptor descriptor, Resource container) {
        final List<Statement> toRemove = new ArrayList<>(connector.find(container, RDF.type, null, contexts(descriptor)));
        toRemove.add(ResourceFactory.createStatement(ResourceFactory.createResource(descriptor.getOwner()
                                                                                              .getIdentifier()
                                                                                              .toString()),
                ResourceFactory.createProperty(descriptor.getProperty().getIdentifier().toString()),
                container)
        );
        return toRemove;
    }
}
