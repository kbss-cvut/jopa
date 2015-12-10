package cz.cvut.kbss.ontodriver.sesame;

import java.util.*;

import org.openrdf.model.Literal;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Assertion.AssertionType;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

class AxiomLoader {

    private final Connector connector;
    private final ValueFactory valueFactory;

    private Map<URI, Assertion> propertyToAssertion;
    private Assertion unspecifiedProperty;

    AxiomLoader(Connector connector, ValueFactory valueFactory) {
        this.connector = connector;
        this.valueFactory = valueFactory;
        this.propertyToAssertion = new HashMap<>();
    }

    Collection<Axiom<?>> loadAxioms(AxiomDescriptor axiomDescriptor) throws SesameDriverException {
        Collection<Statement> statements = findStatements(axiomDescriptor);
        return transformStatementsToAxioms(statements);
    }

    private Collection<Statement> findStatements(AxiomDescriptor descriptor)
            throws SesameDriverException {
        final Collection<Statement> result = new HashSet<>();
        final Resource subject = SesameUtils.toSesameUri(descriptor.getSubject().getIdentifier(),
                valueFactory);
        for (Assertion assertion : descriptor.getAssertions()) {
            final URI context = SesameUtils.toSesameUri(descriptor.getAssertionContext(assertion),
                    valueFactory);
            if (!shouldLoad(assertion, descriptor)) {
                continue;
            }
            final URI property = getPropertyUri(assertion);

            if (context != null) {
                result.addAll(connector.findStatements(subject, property, null, assertion.isInferred(),
                        context));
            } else {
                result.addAll(connector.findStatements(subject, property, null, assertion.isInferred()));
            }
        }
        return result;
    }

    private boolean shouldLoad(Assertion assertion, AxiomDescriptor descriptor) {
        return unspecifiedProperty == null || assertion.getType() == AssertionType.CLASS
                || !sameContext(unspecifiedProperty, assertion, descriptor)
                || (!unspecifiedProperty.isInferred() && assertion.isInferred());
    }

    private boolean sameContext(Assertion assertionOne, Assertion assertionTwo, AxiomDescriptor descriptor) {
        final java.net.URI cOne = descriptor.getAssertionContext(assertionOne);
        final java.net.URI cTwo = descriptor.getAssertionContext(assertionTwo);
        return cOne == cTwo || (cOne != null && cOne.equals(cTwo));
    }

    private URI getPropertyUri(Assertion assertion) {
        if (assertion.equals(Assertion.createUnspecifiedPropertyAssertion(assertion.isInferred()))) {
            this.unspecifiedProperty = assertion;
            return null;
        }
        final URI property = SesameUtils.toSesameUri(assertion.getIdentifier(), valueFactory);
        propertyToAssertion.put(property, assertion);
        return property;
    }

    private List<Axiom<?>> transformStatementsToAxioms(Collection<Statement> statements) {
        final List<Axiom<?>> axioms = new ArrayList<>(statements.size());
        final Map<Resource, NamedResource> subjects = new HashMap<>();
        for (Statement stmt : statements) {
            final Axiom<?> axiom = createAxiom(stmt, subjects);
            if (axiom == null) {
                continue;
            }
            axioms.add(axiom);
        }
        return axioms;
    }

    private Axiom<?> createAxiom(Statement stmt, Map<Resource, NamedResource> knownSubjects) {
        if (!knownSubjects.containsKey(stmt.getSubject())) {
            knownSubjects.put(stmt.getSubject(),
                    NamedResource.create(SesameUtils.toJavaUri(stmt.getSubject())));
        }
        final NamedResource subject = knownSubjects.get(stmt.getSubject());
        Assertion assertion = resolveAssertion(stmt.getPredicate());
        if (SesameUtils.isBlankNode(stmt.getObject())) {
            return null;
        }
        Value<?> val = createValue(assertion.getType(), stmt.getObject());
        if (val == null) {
            return null;
        }
        return new AxiomImpl<>(subject, assertion, val);
    }

    private Assertion resolveAssertion(URI predicate) {
        Assertion assertion = propertyToAssertion.get(predicate);
        if (assertion == null) {
            if (unspecifiedProperty == null) {
                return null;
            } else {
                assertion = unspecifiedProperty;
            }
        }
        // If the property was unspecified, create assertion based on the actual property URI
        if (assertion.getType() == AssertionType.PROPERTY) {
            assertion = Assertion.createPropertyAssertion(
                    SesameUtils.toJavaUri(predicate), assertion.isInferred());
        }
        return assertion;
    }

    private Value<?> createValue(AssertionType assertionType, org.openrdf.model.Value value) {
        switch (assertionType) {
            case ANNOTATION_PROPERTY:
            case DATA_PROPERTY:
                if (!(value instanceof Literal)) {
                    return null;
                }
                return new Value<>(SesameUtils.getDataPropertyValue((Literal) value));
            case CLASS:
                if (!(value instanceof Resource)) {
                    return null;
                }
                return new Value<>(SesameUtils.toJavaUri((Resource) value));
            case OBJECT_PROPERTY:
                if (!(value instanceof Resource)) {
                    return null;
                }
                return new Value<>(NamedResource.create(value.stringValue()));
            case PROPERTY:
                return resolveValue(value);
        }
        return null;
    }

    private Value<?> resolveValue(org.openrdf.model.Value object) {
        if (object instanceof Literal) {
            return new Value<>(SesameUtils.getDataPropertyValue((Literal) object));
        } else {
            return new Value<>(SesameUtils.toJavaUri((Resource) object));
        }
    }

    public Collection<Axiom<?>> loadAxioms(NamedResource individual, boolean includeInferred, java.net.URI context)
            throws SesameDriverException {
        final URI sesameContext = SesameUtils.toSesameUri(context, valueFactory);
        final URI subject = SesameUtils.toSesameUri(individual.getIdentifier(), valueFactory);
        this.unspecifiedProperty = Assertion.createUnspecifiedPropertyAssertion(includeInferred);
        final Collection<Statement> statements;
        if (sesameContext != null) {
            statements = connector.findStatements(subject, null, null, includeInferred, sesameContext);
        } else {
            statements = connector.findStatements(subject, null, null, includeInferred);
        }
        return transformStatementsToAxioms(statements);
    }
}
