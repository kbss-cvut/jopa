/**
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
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
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jOntoDriverProperties;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;

/**
 * Implements loading of inferred statements from the default repository context.
 *
 * @see Rdf4jOntoDriverProperties#INFERENCE_IN_DEFAULT_CONTEXT
 */
public class DefaultContextInferenceStatementLoader extends StatementLoader {

    public DefaultContextInferenceStatementLoader(Connector connector, Resource subject, AxiomBuilder axiomBuilder) {
        super(connector, subject, axiomBuilder);
    }

    @Override
    protected Set<URI> resolveContexts(AxiomDescriptor descriptor, Assertion a) {
        return includeInferred && a.isInferred() ? Collections.emptySet() : super.resolveContexts(descriptor, a);
    }

    @Override
    protected boolean contextMatches(Set<URI> assertionCtx, Statement s, Assertion a) {
        if (includeInferred && a.isInferred()) {
            return true;
        }
        return super.contextMatches(assertionCtx, s, a);
    }

    @Override
    public Collection<Axiom<?>> loadAxioms(Set<URI> contexts) throws Rdf4jDriverException {
        return super.loadAxioms(includeInferred ? Collections.emptySet() : contexts);
    }
}
