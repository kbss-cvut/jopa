package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.model.*;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDF;

import java.net.URI;
import java.util.*;

class TypesHandler {

    private final Connector connector;
    private final ValueFactory valueFactory;

    TypesHandler(Connector connector, ValueFactory valueFactory) {
        this.connector = connector;
        this.valueFactory = valueFactory;
    }

    Set<Axiom<URI>> getTypes(NamedResource individual, URI context, boolean includeInferred) throws SesameDriverException {
        final Collection<Statement> statements = getTypesStatements(individual, context, includeInferred);
        if (statements.isEmpty()) {
            return Collections.emptySet();
        }
        return resolveTypes(individual, includeInferred, statements);
    }

    private Collection<Statement> getTypesStatements(NamedResource individual, URI context, boolean includeInferred) throws SesameDriverException {
        final Resource subject = SesameUtils.toSesameUri(individual.getIdentifier(), valueFactory);
        final org.openrdf.model.URI contextUri = SesameUtils.toSesameUri(context, valueFactory);
        return connector.findStatements(subject, RDF.TYPE, null, includeInferred, contextUri);
    }

    private Set<Axiom<URI>> resolveTypes(NamedResource individual, boolean includeInferred, Collection<Statement> statements) {
        final Set<Axiom<URI>> types = new HashSet<>(statements.size());
        final Assertion clsAssertion = Assertion.createClassAssertion(includeInferred);
        for (Statement stmt : statements) {
            assert stmt.getObject() instanceof Resource;
            final URI type = SesameUtils.toJavaUri((Resource) stmt.getObject());
            if (type == null) {
                // It was a blank node
                continue;
            }
            types.add(new AxiomImpl<>(individual, clsAssertion, new Value<>(type)));
        }
        return types;
    }

    void addTypes(NamedResource individual, URI context, Set<URI> types) throws SesameDriverException {
        final Collection<Statement> statements = prepareSesameStatements(individual, context, types);
        connector.addStatements(statements);
    }

    private Collection<Statement> prepareSesameStatements(NamedResource individual, URI context, Set<URI> types) {
        final org.openrdf.model.URI subject = SesameUtils.toSesameUri(individual.getIdentifier(), valueFactory);
        final org.openrdf.model.URI contextUri = SesameUtils.toSesameUri(context, valueFactory);
        final Collection<Statement> statements = new ArrayList<>(types.size());
        for (URI type : types) {
            statements.add(valueFactory.createStatement(subject, RDF.TYPE, valueFactory.createURI(type.toString()), contextUri));
        }
        return statements;
    }

    void removeTypes(NamedResource individual, URI context, Set<URI> types) throws SesameDriverException {
        final Collection<Statement> statements = prepareSesameStatements(individual, context, types);
        connector.removeStatements(statements);
    }
}
