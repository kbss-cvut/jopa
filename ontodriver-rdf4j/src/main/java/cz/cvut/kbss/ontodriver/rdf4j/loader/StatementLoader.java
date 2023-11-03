/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.rdf4j.loader;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.rdf4j.config.Constants;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.AxiomBuilder;
import cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils;
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
                                           Map<IRI, Assertion> properties) throws Rdf4jDriverException {
        this.loadAll = properties.containsValue(Assertion.createUnspecifiedPropertyAssertion(includeInferred));
        if (properties.size() < loadAllThreshold && !loadAll) {
            return loadOneByOne(descriptor, properties);
        } else {
            return loadAll(descriptor, properties);
        }
    }

    private Collection<Axiom<?>> loadOneByOne(AxiomDescriptor descriptor,
                                              Map<IRI, Assertion> assertions) throws Rdf4jDriverException {
        final Collection<Axiom<?>> result = new HashSet<>();
        for (Map.Entry<IRI, Assertion> e : assertions.entrySet()) {
            final Set<IRI> contexts = resolveContexts(descriptor, e.getValue()).stream()
                                                                               .map(uri -> Rdf4jUtils.toRdf4jIri(uri,
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
                                         Map<IRI, Assertion> properties) throws Rdf4jDriverException {
        final Collection<Statement> statements = connector.findStatements(subject, null, null, includeInferred);
        final Collection<Axiom<?>> result = new HashSet<>(statements.size());
        final Assertion unspecified = Assertion.createUnspecifiedPropertyAssertion(includeInferred);
        for (Statement s : statements) {
            if (!properties.containsKey(s.getPredicate()) && !loadAll) {
                continue;
            }
            final Assertion a = getAssertion(properties, s);
            if (!contextMatches(descriptor.getAssertionContexts(a), s, a) &&
                    !(loadAll && contextMatches(descriptor.getAssertionContexts(unspecified), s, a))) {
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

    protected boolean contextMatches(Set<URI> assertionCtx, Statement s, Assertion a) {
        if (assertionCtx.isEmpty()) {
            // If the assertion should be in default, we don't care about the context of the statement, because
            // the default is a union of all the contexts
            return true;
        }
        final Resource statementContext = s.getContext();
        return statementContext != null && assertionCtx.contains(URI.create(statementContext.stringValue()));
    }

    public Collection<Axiom<?>> loadAxioms(Set<URI> contexts) throws Rdf4jDriverException {
        final Collection<Statement> statements =
                connector.findStatements(subject, null, null, includeInferred, contexts.stream()
                                                                                       .map(uri -> Rdf4jUtils.toRdf4jIri(
                                                                                               uri, vf))
                                                                                       .collect(Collectors.toSet()));
        return statements.stream().map(axiomBuilder::statementToAxiom).collect(Collectors.toSet());
    }
}
