package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.sesame.config.Constants;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;

class ExplicitStatementLoader extends StatementLoader {

    private boolean loadAll;

    ExplicitStatementLoader(AxiomDescriptor descriptor, Connector connector, Resource subject) {
        super(descriptor, connector, subject);
    }

    @Override
    Collection<Statement> loadStatements(Map<URI, Assertion> properties)
            throws SesameDriverException {
        this.loadAll = properties.values().contains(Assertion.createUnspecifiedPropertyAssertion(false));
        if (properties.size() < Constants.DEFAULT_LOAD_ALL_THRESHOLD && !loadAll) {
            return loadOneByOne(properties.values());
        } else {
            return loadAll(properties);
        }
    }

    private Collection<Statement> loadOneByOne(Collection<Assertion> assertions) throws SesameDriverException {
        final Collection<Statement> result = new HashSet<>();
        for (Assertion a : assertions) {
            final URI context = SesameUtils.toSesameUri(descriptor.getAssertionContext(a), vf);
            final URI property = SesameUtils.toSesameUri(a.getIdentifier(), vf);

            if (context != null) {
                result.addAll(connector.findStatements(subject, property, null, false, context));
            } else {
                result.addAll(connector.findStatements(subject, property, null, false));
            }
        }
        return result;
    }

    // TODO This can return collection of axioms, we are already going through all the statements and looking for matching assertions
    private Collection<Statement> loadAll(Map<URI, Assertion> properties) throws SesameDriverException {
        final Collection<Statement> statements = connector.findStatements(subject, null, null, false);
        final Collection<Statement> result = new HashSet<>(statements.size());
        for (Statement s : statements) {
            if (!properties.containsKey(s.getPredicate()) && !loadAll) {
                continue;
            }
            final Assertion a = getAssertion(properties, s);
            if (!contextMatches(a, s) &&
                    !(loadAll && contextMatches(Assertion.createUnspecifiedPropertyAssertion(false), s))) {
                continue;
            }
            result.add(s);
        }
        return result;
    }

    private Assertion getAssertion(Map<URI, Assertion> properties, Statement s) {
        if (properties.containsKey(s.getPredicate())) {
            return properties.get(s.getPredicate());
        }
        return Assertion.createUnspecifiedPropertyAssertion(false);
    }

    private boolean contextMatches(Assertion a, Statement s) {
        final java.net.URI assertionCtx = descriptor.getAssertionContext(a);
        final Resource statementContext = s.getContext();
        if (assertionCtx == null) {
            // If the assertion should be in default, we don't care about the context of the statement, because
            // the default is a union of all the contexts
            return true;
        }
        return statementContext != null && assertionCtx.toString().equals(statementContext.stringValue());
    }
}
