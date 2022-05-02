/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame.loader;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.sesame.config.Constants;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.sesame.util.AxiomBuilder;
import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;

import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class StatementLoader {

    private final Connector connector;
    private final Resource subject;
    private final ValueFactory vf;
    private final AxiomBuilder axiomBuilder;

    private int loadAllThreshold = Constants.DEFAULT_LOAD_ALL_THRESHOLD;
    private boolean loadAll;
    boolean includeInferred;

    public StatementLoader(Connector connector, Resource subject, AxiomBuilder axiomBuilder) {
        this.connector = connector;
        this.vf = connector.getValueFactory();
        this.subject = subject;
        this.axiomBuilder = axiomBuilder;
    }

    public void setLoadAllThreshold(int loadAllThreshold) {
        this.loadAllThreshold = loadAllThreshold;
    }

    public void setIncludeInferred(boolean includeInferred) {
        this.includeInferred = includeInferred;
    }

    public Collection<Axiom<?>> loadAxioms(AxiomDescriptor descriptor,
                                           Map<IRI, Assertion> properties) throws SesameDriverException {
        this.loadAll = properties.containsValue(Assertion.createUnspecifiedPropertyAssertion(includeInferred));
        if (properties.size() < loadAllThreshold && !loadAll) {
            return loadOneByOne(descriptor, properties);
        } else {
            return loadAll(descriptor, properties);
        }
    }

    private Collection<Axiom<?>> loadOneByOne(AxiomDescriptor descriptor,
                                              Map<IRI, Assertion> assertions) throws SesameDriverException {
        final Collection<Axiom<?>> result = new HashSet<>();
        for (Map.Entry<IRI, Assertion> e : assertions.entrySet()) {
            final Set<IRI> contexts = resolveContexts(descriptor, e.getValue()).stream()
                                                                               .map(uri -> SesameUtils.toSesameIri(uri,
                                                                                                                   vf))
                                                                               .collect(Collectors.toSet());

            final Collection<Statement> statements;
            statements = connector.findStatements(subject, e.getKey(), null, includeInferred, contexts);
            for (Statement s : statements) {
                final Axiom<?> axiom = axiomBuilder.statementToAxiom(s, e.getValue());
                if (axiom != null) {
                    result.add(axiom);
                }
            }
        }
        return result;
    }

    protected Set<URI> resolveContexts(AxiomDescriptor descriptor, Assertion a) {
        return descriptor.getAssertionContexts(a);
    }

    private Collection<Axiom<?>> loadAll(AxiomDescriptor descriptor,
                                         Map<IRI, Assertion> properties) throws SesameDriverException {
        final Collection<Statement> statements = connector.findStatements(subject, null, null, includeInferred);
        final Collection<Axiom<?>> result = new HashSet<>(statements.size());
        final Assertion unspecified = Assertion.createUnspecifiedPropertyAssertion(includeInferred);
        for (Statement s : statements) {
            if (!properties.containsKey(s.getPredicate()) && !loadAll) {
                continue;
            }
            final Assertion a = getAssertion(properties, s);
            if (!contextMatches(resolveContexts(descriptor, a), s) &&
                    !(loadAll && contextMatches(resolveContexts(descriptor, unspecified), s))) {
                continue;
            }
            final Axiom<?> axiom = axiomBuilder.statementToAxiom(s);
            if (axiom != null) {
                result.add(axiom);
            }
        }
        return result;
    }

    private Assertion getAssertion(Map<IRI, Assertion> properties, Statement s) {
        if (properties.containsKey(s.getPredicate())) {
            return properties.get(s.getPredicate());
        }
        return Assertion.createUnspecifiedPropertyAssertion(includeInferred);
    }

    private boolean contextMatches(Set<URI> assertionCtx, Statement s) {
        if (assertionCtx.isEmpty()) {
            // If the assertion should be in default, we don't care about the context of the statement, because
            // the default is a union of all the contexts
            return true;
        }
        final Resource statementContext = s.getContext();
        return statementContext != null && assertionCtx.contains(URI.create(statementContext.stringValue()));
    }

    public Collection<Axiom<?>> loadAxioms(Set<URI> contexts) throws SesameDriverException {
        final Collection<Statement> statements =
                connector.findStatements(subject, null, null, includeInferred, contexts.stream()
                                                                                       .map(uri -> SesameUtils.toSesameIri(
                                                                                               uri, vf))
                                                                                       .collect(Collectors.toSet()));
        return statements.stream().map(axiomBuilder::statementToAxiom).collect(Collectors.toSet());
    }
}
