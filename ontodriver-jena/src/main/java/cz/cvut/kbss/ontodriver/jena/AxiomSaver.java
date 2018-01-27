package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.util.JenaUtils;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.rdf.model.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

class AxiomSaver {

    private final StorageConnector connector;

    AxiomSaver(StorageConnector connector) {
        this.connector = connector;
    }

    void saveAxioms(AxiomValueDescriptor descriptor) {
        final Resource subject = ResourceFactory.createResource(descriptor.getSubject().getIdentifier().toString());
        // TODO This does not deal with contexts
        final List<Statement> statements = new ArrayList<>();
        for (Assertion a : descriptor.getAssertions()) {
            final Property property = ResourceFactory.createProperty(a.getIdentifier().toString());
            if (a.getType() == Assertion.AssertionType.OBJECT_PROPERTY || a.isClassAssertion()) {
                statements.addAll(descriptor.getAssertionValues(a).stream().map(v -> {
                    final Resource value = ResourceFactory.createResource(v.stringValue());
                    return ResourceFactory.createStatement(subject, property, value);
                }).collect(Collectors.toList()));
            } else if (a.getType() == Assertion.AssertionType.DATA_PROPERTY) {
                statements
                        .addAll(dataPropertyValuesToStatements(descriptor.getAssertionValues(a), subject, a, property));
            } else {
                statements.addAll(descriptor.getAssertionValues(a).stream().map(v -> {
                    final RDFNode value = resolveValue(a, v);
                    return ResourceFactory.createStatement(subject, property, value);
                }).collect(Collectors.toList()));
            }
        }
        connector.add(statements);
    }

    private List<Statement> dataPropertyValuesToStatements(Collection<Value<?>> values, Resource subject, Assertion a,
                                                           Property property) {
        return values.stream().map(v -> {
            final Literal value;
            if (a.hasLanguage()) {
                value = ResourceFactory.createLangLiteral(v.stringValue(), a.getLanguage());
            } else {
                value = ResourceFactory.createTypedLiteral(v.getValue());
            }
            return ResourceFactory.createStatement(subject, property, value);
        }).collect(Collectors.toList());
    }

    private RDFNode resolveValue(Assertion a, Value<?> value) {
        if (JenaUtils.isResourceIdentifier(value.getValue())) {
            return ResourceFactory.createResource(value.stringValue());
        } else {
            return a.hasLanguage() ? ResourceFactory.createLangLiteral(value.stringValue(), a.getLanguage()) :
                    ResourceFactory.createTypedLiteral(value.getValue());
        }
    }
}
