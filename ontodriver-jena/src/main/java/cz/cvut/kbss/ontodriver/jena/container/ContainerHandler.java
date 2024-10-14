package cz.cvut.kbss.ontodriver.jena.container;

import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.util.JenaUtils;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

public class ContainerHandler {

    private static final String MEMBERSHIP_PROPERTY_URI_BASE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#_";
    private static final int MEMBERSHIP_PROPERTY_URI_BASE_LENGTH = MEMBERSHIP_PROPERTY_URI_BASE.length();

    private final StorageConnector connector;

    public ContainerHandler(StorageConnector connector) {this.connector = connector;}

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
        final Collection<String> contexts = descriptor.getContext() != null ? List.of(descriptor.getContext().toString()) : List.of();
        final Collection<Statement> containerStatements = connector.find(subject, property, null, contexts);
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

    private List<Axiom<?>> readContainerContent(Resource container, ContainerDescriptor descriptor) throws JenaDriverException {
        final Collection<String> contexts = descriptor.getContext() != null ? List.of(descriptor.getContext().toString()) : List.of();
        final Collection<Statement> content = connector.find(container, null, null, contexts);
        return  content.stream()
                      .filter(s -> s.getPredicate().getURI().startsWith(MEMBERSHIP_PROPERTY_URI_BASE))
                      .sorted(ContainerHandler::statementComparator)
                .map(s -> {
                    final RDFNode element = s.getObject();
                    if (element.isResource()) {
                        return new AxiomImpl<>(descriptor.getOwner(), descriptor.getProperty(), new Value<>(NamedResource.create(element.asResource().getURI())));
                    } else {
                        return (Axiom<?>) new AxiomImpl<>(descriptor.getOwner(), descriptor.getProperty(), new Value<>(JenaUtils.literalToValue(element.asLiteral())));
                    }
                }).toList();

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
}
