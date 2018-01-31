package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.util.JenaUtils;
import cz.cvut.kbss.ontodriver.model.*;
import org.apache.jena.rdf.model.*;

import java.net.URI;
import java.util.*;

// TODO The loader does not support inferred statements, yet
class AxiomLoader {

    private final StorageConnector connector;

    private Map<String, Assertion> assertedProperties;
    private Assertion unspecifiedProperty;

    AxiomLoader(StorageConnector connector) {
        this.connector = connector;
    }

    boolean contains(Axiom<?> axiom, URI context) {
        final Resource subject = ResourceFactory.createResource(axiom.getSubject().getIdentifier().toString());
        final Property property = ResourceFactory.createProperty(axiom.getAssertion().getIdentifier().toString());
        final RDFNode object = JenaUtils.valueToRdfNode(axiom.getAssertion(), axiom.getValue());
        if (context != null) {
            return connector.contains(subject, property, object, context.toString());
        } else {
            return connector.contains(subject, property, object);
        }
    }

    Collection<Axiom<?>> find(AxiomDescriptor descriptor) {
        final Resource subject = ResourceFactory.createResource(descriptor.getSubject().getIdentifier().toString());
        final Collection<Statement> statements = connector.find(subject, null, null);
        final List<Axiom<?>> result = new ArrayList<>(statements.size());
        processProperties(descriptor);
        for (Statement statement : statements) {
            final Property property = statement.getPredicate();
            if (!assertedProperties.containsKey(property.getURI()) && unspecifiedProperty == null) {
                continue;
            }
            final Assertion a =
                    assertedProperties.containsKey(property.getURI()) ? assertedProperties.get(property.getURI()) :
                            createAssertionForStatement(property, statement.getObject());
            final Optional<Value<?>> value = resolveValue(a, statement.getObject());
            value.ifPresent(v -> result.add(new AxiomImpl<>(descriptor.getSubject(), a, v)));
        }
        return result;
    }

    private void processProperties(AxiomDescriptor descriptor) {
        this.assertedProperties = new HashMap<>(descriptor.getAssertions().size());
        final Assertion unspecified = Assertion.createUnspecifiedPropertyAssertion(false);
        for (Assertion a : descriptor.getAssertions()) {
            if (a.isInferred()) {
                continue;   // For now
            }
            if (a.equals(unspecified)) {
                this.unspecifiedProperty = a;
                continue;
            }
            assertedProperties.put(a.getIdentifier().toString(), a);
        }
    }

    private Optional<Value<?>> resolveValue(Assertion assertion, RDFNode object) {
        // TODO We shouldn't load types for unspecified property, they should be loaded only for class assertion
        if (object.isResource()) {
            if (object.isAnon() || assertion.getType() == Assertion.AssertionType.DATA_PROPERTY) {
                return Optional.empty();
            }
            return Optional.of(new Value<>(NamedResource.create(object.asResource().getURI())));
        } else {
            if (shouldSkipLiteral(assertion, object)) {
                return Optional.empty();
            }
            return Optional.of(new Value<>(object.asLiteral().getValue()));
        }
    }

    private boolean shouldSkipLiteral(Assertion assertion, RDFNode object) {
        return assertion.getType() == Assertion.AssertionType.OBJECT_PROPERTY || assertion.hasLanguage() && !assertion
                .getLanguage().equals(object.asLiteral().getLanguage());
    }

    private Assertion createAssertionForStatement(Property property, RDFNode value) {
        if (value.isResource()) {
            return Assertion
                    .createObjectPropertyAssertion(URI.create(property.getURI()), unspecifiedProperty.isInferred());
        } else {
            return Assertion
                    .createDataPropertyAssertion(URI.create(property.getURI()), unspecifiedProperty.isInferred());
        }
    }
}
