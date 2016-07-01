package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.openrdf.model.*;

import java.util.Collection;
import java.util.Map;

abstract class StatementLoader {

    final AxiomDescriptor descriptor;
    final Connector connector;
    final Resource subject;
    final ValueFactory vf;

    StatementLoader(AxiomDescriptor descriptor, Connector connector, Resource subject) {
        this.descriptor = descriptor;
        this.connector = connector;
        this.vf = connector.getValueFactory();
        this.subject = subject;
    }

    abstract Collection<Statement> loadStatements(Map<URI, Assertion> properties) throws SesameDriverException;

    Axiom<?> createAxiom(Statement stmt, Map<URI, Assertion> propertyToAssertion) {
        assert stmt.getSubject().stringValue().equals(descriptor.getSubject().getIdentifier().toString());

        final NamedResource subject = descriptor.getSubject();
        Assertion assertion = resolveAssertion(stmt.getPredicate(), propertyToAssertion);
        if (SesameUtils.isBlankNode(stmt.getObject()) || assertion == null) {
            return null;
        }
        Value<?> val = createValue(assertion.getType(), stmt.getObject());
        if (val == null) {
            return null;
        }
        return new AxiomImpl<>(subject, assertion, val);
    }

    private Assertion resolveAssertion(URI predicate, Map<URI, Assertion> propertyToAssertion) {
        Assertion assertion = propertyToAssertion.get(predicate);
        // If the property was unspecified, create assertion based on the actual property URI
        if (assertion == null || assertion.getType() == Assertion.AssertionType.PROPERTY) {
            assertion = Assertion.createPropertyAssertion(SesameUtils.toJavaUri(predicate), false);
            // TODO
        }
        return assertion;
    }

    private Value<?> createValue(Assertion.AssertionType assertionType, org.openrdf.model.Value value) {
        switch (assertionType) {
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
            case ANNOTATION_PROPERTY:   // Intentional fall-through
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
}
