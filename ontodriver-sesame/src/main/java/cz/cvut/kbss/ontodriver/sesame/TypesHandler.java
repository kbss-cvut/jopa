package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.model.*;
import org.openrdf.model.BNode;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDF;

import java.net.URI;
import java.util.*;

/**
 * Created by ledvima1 on 26.11.14.
 */
class TypesHandler {

    private final Connector connector;
    private final ValueFactory valueFactory;

    TypesHandler(Connector connector, ValueFactory valueFactory) {
        this.connector = connector;
        this.valueFactory = valueFactory;
    }

    Set<Axiom<URI>> getTypes(NamedResource individual, URI context, boolean includeInferred) throws SesameDriverException {
        final Resource subject = SesameUtils.toSesameUri(individual.getIdentifier(), valueFactory);
        final org.openrdf.model.URI contextUri = SesameUtils.toSesameUri(context, valueFactory);
        final Collection<Statement> statements = connector.findStatements(subject, RDF.TYPE, null, includeInferred, contextUri);
        if (statements.isEmpty()) {
            return Collections.emptySet();
        }
        final Set<Axiom<URI>> types = resolveTypes(individual, includeInferred, statements);
        return types;
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
            types.add(new AxiomImpl<URI>(individual, clsAssertion, new Value<URI>(type)));
        }
        return types;
    }

    void persistTypes(NamedResource individual, URI context, Set<URI> types) throws SesameDriverException {
        final org.openrdf.model.URI subject = SesameUtils.toSesameUri(individual.getIdentifier(), valueFactory);
        final org.openrdf.model.URI contextUri = SesameUtils.toSesameUri(context, valueFactory);
        final Collection<Statement> statements = new ArrayList<>(types.size());
        for (URI type : types) {
            statements.add(valueFactory.createStatement(subject, RDF.TYPE, valueFactory.createURI(type.toString()), contextUri));
        }
        connector.addStatements(statements);
    }

    void updateTypes(NamedResource individual, URI context, Set<URI> types) {
        // TODO
    }
}
