/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.rdf4j.loader;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.AxiomBuilder;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;

import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * Statement loader for GraphDB repositories.
 * <p>
 * It differs from the basic {@link StatementLoader} in the way inferred statements are loaded. This is because GraphDB
 * does not store inferred statements in the same context as the statements they are inferred from (as RDF4J does), but
 * instead has a special {@code implicit} context for them.
 */
public class GraphDBStatementLoader extends StatementLoader {

    /**
     * Repository pseudo-context used by GraphDB to store inferred statements.
     */
    static final URI GRAPHDB_IMPLICIT_CONTEXT = URI.create("http://www.ontotext.com/implicit");

    /**
     * Repository pseudo-context used by GraphDB to store explicit statements in the default context.
     */
    static final URI GRAPHDB_EXPLICIT_CONTEXT = URI.create("http://www.ontotext.com/explicit");

    public GraphDBStatementLoader(Connector connector, Resource subject, AxiomBuilder axiomBuilder) {
        super(connector, subject, axiomBuilder);
    }

    @Override
    protected Set<URI> resolveContexts(AxiomDescriptor descriptor, Assertion a) {
        final Set<URI> contexts = new HashSet<>(super.resolveContexts(descriptor, a));
        if (includeInferred) {
            contexts.add(GRAPHDB_IMPLICIT_CONTEXT);
            // Add explicit context for the cases when an explicit statement in default hides the inferred one.
            // This is just to make the behavior consistent with contextMatches
            contexts.add(GRAPHDB_EXPLICIT_CONTEXT);
        }
        return contexts;
    }

    @Override
    protected boolean contextMatches(Set<URI> assertionCtx, Statement s, Assertion a) {
        if (includeInferred && a.isInferred() && s.getContext() == null) {
            // If the statement is inferred, its context is null in GraphDB (although when querying, one can ask the implicit context)
            return true;
        }
        return super.contextMatches(assertionCtx, s, a);
    }

    @Override
    public Collection<Axiom<?>> loadAxioms(Set<URI> contexts) throws Rdf4jDriverException {
        if (includeInferred) {
            final Set<URI> contextsToUse = new HashSet<>(contexts);
            contextsToUse.add(GRAPHDB_IMPLICIT_CONTEXT);
            contextsToUse.add(GRAPHDB_EXPLICIT_CONTEXT);
            return super.loadAxioms(contextsToUse);
        }
        return super.loadAxioms(contexts);
    }
}
