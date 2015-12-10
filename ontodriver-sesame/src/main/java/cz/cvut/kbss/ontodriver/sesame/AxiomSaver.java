package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;

import java.net.URI;
import java.util.*;

class AxiomSaver {

    private final Connector connector;
    private final ValueFactory valueFactory;
    private final SesameValueConverter valueConverter;

    AxiomSaver(Connector connector, ValueFactory valueFactory, String language) {
        this.connector = connector;
        this.valueFactory = valueFactory;
        this.valueConverter = new SesameValueConverter(valueFactory, language);
    }

    void persistAxioms(AxiomValueDescriptor axiomDescriptor) throws SesameDriverException {
        final List<Statement> statements = new ArrayList<>();
        for (Assertion assertion : axiomDescriptor.getAssertions()) {
            statements.addAll(createSesameStatements(axiomDescriptor.getSubject(), assertion,
                    axiomDescriptor.getAssertionValues(assertion),
                    axiomDescriptor.getAssertionContext(assertion)));
        }
        if (!statements.isEmpty()) {
            connector.addStatements(statements);
        }
    }

    void persistAxioms(NamedResource subject, Map<Assertion, Set<Value<?>>> values, URI context) throws SesameDriverException {
        final List<Statement> statements = new ArrayList<>();
        for (Map.Entry<Assertion, Set<Value<?>>> entry : values.entrySet()) {
            statements.addAll(createSesameStatements(subject, entry.getKey(), entry.getValue(), context));
        }
        if (!statements.isEmpty()) {
            connector.addStatements(statements);
        }
    }

    private Collection<? extends Statement> createSesameStatements(NamedResource subject,
                                                                   Assertion assertion, Collection<Value<?>> assertionValues, URI assertionContext)
            throws SesameDriverException {
        final List<Statement> statements = new ArrayList<>(assertionValues.size());

        final org.openrdf.model.Resource subjectUri = SesameUtils.toSesameUri(
                subject.getIdentifier(), valueFactory);
        final org.openrdf.model.URI property = SesameUtils.toSesameUri(assertion.getIdentifier(),
                valueFactory);
        final org.openrdf.model.URI context = assertionContext != null ? SesameUtils.toSesameUri(
                assertionContext, valueFactory) : null;
        for (Value<?> val : assertionValues) {
            if (val == Value.nullValue()) {
                continue;
            }
            org.openrdf.model.Value value = valueConverter.toSesameValue(assertion, val);
            statements.add(createStatement(subjectUri, property, value, context));
        }
        return statements;
    }

    private Statement createStatement(Resource subject, org.openrdf.model.URI property,
                                      org.openrdf.model.Value value, org.openrdf.model.URI context) {
        if (context != null) {
            return valueFactory.createStatement(subject, property, value, context);
        } else {
            return valueFactory.createStatement(subject, property, value);
        }
    }
}
