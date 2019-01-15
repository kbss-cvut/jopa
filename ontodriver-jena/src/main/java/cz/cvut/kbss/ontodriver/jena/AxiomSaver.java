package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.util.JenaUtils;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.rdf.model.*;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

class AxiomSaver {

    private final StorageConnector connector;

    AxiomSaver(StorageConnector connector) {
        this.connector = connector;
    }

    /**
     * Persists statements corresponding to axioms specified in the descriptor.
     *
     * @param descriptor Data container
     */
    void saveAxioms(AxiomValueDescriptor descriptor) {
        final Resource subject = ResourceFactory.createResource(descriptor.getSubject().getIdentifier().toString());
        final Map<String, List<Statement>> statements = new HashMap<>();
        for (Assertion a : descriptor.getAssertions()) {
            final URI context = descriptor.getAssertionContext(a);
            final String strContext = context != null ? context.toString() : null;
            statements.putIfAbsent(strContext, new ArrayList<>());
            statements.get(strContext).addAll(transformToStatements(a, descriptor.getAssertionValues(a), subject));
        }
        statements.forEach((ctx, toAdd) -> connector.add(toAdd, ctx));
    }

    private static List<Statement> transformToStatements(Assertion assertion, Collection<Value<?>> values, Resource subject) {
        final Property property = ResourceFactory.createProperty(assertion.getIdentifier().toString());
        switch (assertion.getType()) {
            // Intentional fall-through
            case CLASS:
            case OBJECT_PROPERTY:
                return values.stream().filter(v -> v != Value.nullValue()).map(v -> ResourceFactory
                        .createStatement(subject, property, ResourceFactory.createResource(v.stringValue())))
                             .collect(Collectors.toList());
            case DATA_PROPERTY:
                return dataPropertyValuesToStatements(values, subject, assertion, property);
            default:
                return values.stream().filter(v -> v != Value.nullValue())
                             .map(v -> ResourceFactory
                                     .createStatement(subject, property, JenaUtils.valueToRdfNode(assertion, v)))
                             .collect(Collectors.toList());

        }
    }

    private static List<Statement> dataPropertyValuesToStatements(Collection<Value<?>> values, Resource subject, Assertion a,
                                                           Property property) {
        return values.stream().filter(v -> v != Value.nullValue()).map(v -> {
            final Literal value;
            if (a.hasLanguage() && v.getValue() instanceof String) {
                value = ResourceFactory.createLangLiteral(v.stringValue(), a.getLanguage());
            } else if (v.getValue() instanceof Date) {
                // Jena does not like java.util.Date, it works with Calendar values
                final GregorianCalendar cal = new GregorianCalendar();
                cal.setTime((Date) v.getValue());
                value = ResourceFactory.createTypedLiteral(cal);
            } else {
                value = ResourceFactory.createTypedLiteral(v.getValue());
            }
            return ResourceFactory.createStatement(subject, property, value);
        }).collect(Collectors.toList());
    }

    /**
     * Persists statements corresponding to the specified data.
     *
     * @param subject    Statement subject
     * @param properties Assertion to value map, which will be transformed to property to statement object
     * @param context    Context into which statements should be inserted
     */
    void saveAxioms(NamedResource subject, Map<Assertion, Set<Value<?>>> properties, URI context) {
        final Resource resource = ResourceFactory.createResource(subject.getIdentifier().toString());
        final List<Statement> statements = new ArrayList<>(properties.size());
        for (Map.Entry<Assertion, Set<Value<?>>> e : properties.entrySet()) {
            statements.addAll(transformToStatements(e.getKey(), e.getValue(), resource));
        }
        connector.add(statements, context != null ? context.toString() : null);
    }
}
