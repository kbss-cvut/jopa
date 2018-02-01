package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.util.JenaUtils;
import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.rdf.model.*;

import java.net.URI;
import java.util.*;

import static cz.cvut.kbss.ontodriver.model.Assertion.createDataPropertyAssertion;
import static cz.cvut.kbss.ontodriver.model.Assertion.createObjectPropertyAssertion;

// TODO The loader does not support inferred statements, yet
class AxiomLoader {

    private static final Assertion UNSPECIFIED_ASSERTION = Assertion.createUnspecifiedPropertyAssertion(false);

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
        mapProperties(descriptor);
        final Resource subject = ResourceFactory.createResource(descriptor.getSubject().getIdentifier().toString());
        final Collection<Statement> statements = findInternal(subject, null, descriptor.getSubjectContext());
        final List<Axiom<?>> result = transformStatementsToAxioms(descriptor, statements);
        result.addAll(loadAxiomsForPropertiesInContext(descriptor, subject));
        return result;
    }

    private void mapProperties(AxiomDescriptor descriptor) {
        this.assertedProperties = new HashMap<>(descriptor.getAssertions().size());
        for (Assertion a : descriptor.getAssertions()) {
            if (a.isInferred()) {
                continue;   // For now
            }
            if (a.equals(UNSPECIFIED_ASSERTION)) {
                this.unspecifiedProperty = a;
                continue;
            }
            assertedProperties.put(a.getIdentifier().toString(), a);
        }
    }

    private Collection<Statement> findInternal(Resource subject, Property property, URI context) {
        if (context != null) {
            return connector.find(subject, property, null, context.toString());
        } else {
            return connector.find(subject, property, null);
        }
    }

    private List<Axiom<?>> transformStatementsToAxioms(AxiomDescriptor descriptor, Collection<Statement> statements) {
        final List<Axiom<?>> axioms = new ArrayList<>(statements.size());
        for (Statement statement : statements) {
            final Property property = statement.getPredicate();
            if (shouldSkipProperty(property, descriptor)) {
                continue;
            }
            final Assertion a =
                    assertedProperties.containsKey(property.getURI()) ? assertedProperties.get(property.getURI()) :
                            createAssertionForStatement(property, statement.getObject());
            final Optional<Value<?>> value = resolveValue(a, statement.getObject());
            value.ifPresent(v -> axioms.add(new AxiomImpl<>(descriptor.getSubject(), a, v)));
        }
        return axioms;
    }

    private boolean shouldSkipProperty(Property property, AxiomDescriptor descriptor) {
        final String propertyUri = property.getURI();
        // If the property is not mapped and either there is not the unspecified property or the property is rdf:type,
        // which is handled by Types
        if (!assertedProperties.containsKey(propertyUri) && (unspecifiedProperty == null || propertyUri
                .equals(Vocabulary.RDF_TYPE))) {
            return true;
        }
        final Assertion a = assertedProperties.getOrDefault(propertyUri, unspecifiedProperty);
        return !assertionContextSameAsSubject(descriptor.getSubjectContext(), descriptor.getAssertionContext(a));
    }

    private Optional<Value<?>> resolveValue(Assertion assertion, RDFNode object) {
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
        assert unspecifiedProperty != null;
        if (value.isResource()) {
            return createObjectPropertyAssertion(URI.create(property.getURI()), unspecifiedProperty.isInferred());
        } else {
            return createDataPropertyAssertion(URI.create(property.getURI()), unspecifiedProperty.isInferred());
        }
    }

    private List<Axiom<?>> loadAxiomsForPropertiesInContext(AxiomDescriptor descriptor, Resource subject) {
        final List<Axiom<?>> axioms = new ArrayList<>();
        for (Assertion a : assertedProperties.values()) {
            final URI assertionCtx = descriptor.getAssertionContext(a);
            if (assertionContextSameAsSubject(descriptor.getSubjectContext(), assertionCtx)) {
                continue;
            }
            final Property property = ResourceFactory.createProperty(a.getIdentifier().toString());
            final Collection<Statement> statements = findInternal(subject, property, assertionCtx);
            statements.forEach(statement -> {
                final Optional<Value<?>> value = resolveValue(a, statement.getObject());
                value.ifPresent(v -> axioms.add(new AxiomImpl<>(descriptor.getSubject(), a, v)));
            });
        }
        if (unspecifiedProperty != null && !assertionContextSameAsSubject(descriptor.getSubjectContext(),
                descriptor.getAssertionContext(unspecifiedProperty))) {
            final Collection<Statement> statements =
                    findInternal(subject, null, descriptor.getAssertionContext(unspecifiedProperty));
            for (Statement s : statements) {
                final Assertion a = createAssertionForStatement(s.getPredicate(), s.getObject());
                final Optional<Value<?>> value = resolveValue(a, s.getObject());
                value.ifPresent(v -> axioms.add(new AxiomImpl<>(descriptor.getSubject(), a, v)));
            }
        }
        return axioms;
    }

    private boolean assertionContextSameAsSubject(URI subjectCtx, URI assertionCtx) {
        return assertionCtx == null && subjectCtx == null || (subjectCtx != null && assertionCtx != null && subjectCtx
                .equals(assertionCtx));
    }
}
