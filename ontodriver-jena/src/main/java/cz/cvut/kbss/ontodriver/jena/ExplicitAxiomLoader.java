package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.rdf.model.*;

import java.net.URI;
import java.util.*;

class ExplicitAxiomLoader extends AbstractAxiomLoader {

    private static final Assertion UNSPECIFIED_ASSERTION = Assertion.createUnspecifiedPropertyAssertion(false);

    private final StorageConnector connector;

    private Map<String, Assertion> assertedProperties;
    private Assertion unspecifiedProperty;

    ExplicitAxiomLoader(StorageConnector connector) {
        this.connector = connector;
    }

    @Override
    boolean contains(Resource subject, Property property, RDFNode object, URI context) {
        return connector.contains(subject, property, object, context != null ? context.toString() : null);
    }

    @Override
    Collection<Axiom<?>> find(AxiomDescriptor descriptor, Map<String, Assertion> assertions) {
        this.assertedProperties = assertions;
        this.unspecifiedProperty = resolveUnspecifiedProperty();
        final Resource subject = ResourceFactory.createResource(descriptor.getSubject().getIdentifier().toString());
        final Collection<Statement> statements = findStatements(subject, null, descriptor.getSubjectContext());
        final List<Axiom<?>> result = transformStatementsToAxioms(descriptor, statements);
        result.addAll(loadAxiomsForPropertiesInContext(descriptor, subject));
        return result;
    }

    private Assertion resolveUnspecifiedProperty() {
        final Optional<Assertion> unspecified =
                assertedProperties.values().stream().filter(a -> a.equals(UNSPECIFIED_ASSERTION)).findAny();
        return unspecified.orElse(null);
    }

    @Override
    Collection<Statement> findStatements(Resource subject, Property property, URI context) {
        return connector.find(subject, property, null, context != null ? context.toString() : null);
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
                            createAssertionForStatement(statement);
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

    private List<Axiom<?>> loadAxiomsForPropertiesInContext(AxiomDescriptor descriptor, Resource subject) {
        final List<Axiom<?>> axioms = new ArrayList<>();
        for (Assertion a : assertedProperties.values()) {
            final URI assertionCtx = descriptor.getAssertionContext(a);
            if (assertionContextSameAsSubject(descriptor.getSubjectContext(), assertionCtx)) {
                continue;
            }
            final Property property = ResourceFactory.createProperty(a.getIdentifier().toString());
            final Collection<Statement> statements = findStatements(subject, property, assertionCtx);
            statements.forEach(statement -> {
                final Optional<Value<?>> value = resolveValue(a, statement.getObject());
                value.ifPresent(v -> axioms.add(new AxiomImpl<>(descriptor.getSubject(), a, v)));
            });
        }
        if (unspecifiedProperty != null && !assertionContextSameAsSubject(descriptor.getSubjectContext(),
                descriptor.getAssertionContext(unspecifiedProperty))) {
            final Collection<Statement> statements =
                    findStatements(subject, null, descriptor.getAssertionContext(unspecifiedProperty));
            for (Statement s : statements) {
                final Assertion a = createAssertionForStatement(s);
                final Optional<Value<?>> value = resolveValue(a, s.getObject());
                value.ifPresent(v -> axioms.add(new AxiomImpl<>(descriptor.getSubject(), a, v)));
            }
        }
        return axioms;
    }

    private static boolean assertionContextSameAsSubject(URI subjectCtx, URI assertionCtx) {
        return assertionCtx == null && subjectCtx == null || (subjectCtx != null && subjectCtx.equals(assertionCtx));
    }
}
