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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.sesame.config.RuntimeConfiguration;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.sesame.loader.StatementLoader;
import cz.cvut.kbss.ontodriver.sesame.util.AxiomBuilder;
import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import org.eclipse.rdf4j.model.IRI;

import java.util.*;

class AxiomLoader {

    private final Connector connector;

    private final Map<IRI, Assertion> propertyToAssertion;
    private Map<IRI, Assertion> explicitAssertions;
    private Map<IRI, Assertion> inferredAssertions;

    private final RuntimeConfiguration config;

    AxiomLoader(Connector connector, RuntimeConfiguration config) {
        this.connector = connector;
        this.propertyToAssertion = new HashMap<>();
        this.config = config;
    }

    Collection<Axiom<?>> loadAxioms(AxiomDescriptor axiomDescriptor) throws SesameDriverException {
        return findStatements(axiomDescriptor);
    }

    private Collection<Axiom<?>> findStatements(AxiomDescriptor descriptor) throws SesameDriverException {
        final Collection<Axiom<?>> result = new HashSet<>();
        final Assertion unspecified = processAssertions(descriptor);
        final AxiomBuilder axiomBuilder = new AxiomBuilder(descriptor.getSubject(), propertyToAssertion, unspecified);
        final StatementLoader statementLoader = createLoader(descriptor.getSubject(), axiomBuilder);
        statementLoader.setLoadAllThreshold(config.getLoadAllThreshold());
        if (unspecified == null || !unspecified.isInferred()) {
            statementLoader.setIncludeInferred(false);
            result.addAll(statementLoader.loadAxioms(descriptor, explicitAssertions));
        }
        statementLoader.setIncludeInferred(true);
        result.addAll(statementLoader.loadAxioms(descriptor, inferredAssertions));
        return result;
    }

    private StatementLoader createLoader(NamedResource individual, AxiomBuilder axiomBuilder) {
        final IRI subject = SesameUtils.toSesameIri(individual.getIdentifier(), connector.getValueFactory());
        return config.getStatementLoaderFactory().create(connector, subject, axiomBuilder);
    }

    /**
     * Processes assertions in the specified descriptor.
     * <p>
     * Splits them into explicit and inferred and returns unspecified property, if it is present in the descriptor.
     *
     * @param descriptor The descriptor to process
     * @return Unspecified property, if it is present
     */
    private Assertion processAssertions(AxiomDescriptor descriptor) {
        final Set<Assertion> assertions = descriptor.getAssertions();
        this.explicitAssertions = new HashMap<>(assertions.size());
        this.inferredAssertions = new HashMap<>(assertions.size());
        Assertion unspecified = null;
        for (Assertion a : assertions) {
            final IRI property = SesameUtils.toSesameIri(a.getIdentifier(), connector.getValueFactory());
            propertyToAssertion.put(property, a);
            if (a.equals(Assertion.createUnspecifiedPropertyAssertion(a.isInferred()))) {
                unspecified = a;
            }
            if (a.isInferred()) {
                inferredAssertions.put(property, a);
            } else {
                explicitAssertions.put(property, a);
            }
        }
        return unspecified;
    }

    Collection<Axiom<?>> loadAxioms(NamedResource individual, boolean includeInferred, java.net.URI context)
            throws SesameDriverException {
        final AxiomBuilder axiomBuilder = new AxiomBuilder(individual, Collections.emptyMap(),
                Assertion.createUnspecifiedPropertyAssertion(includeInferred));
        final StatementLoader loader = createLoader(individual, axiomBuilder);
        return loader.loadAxioms(context != null ? Collections.singleton(context) : Collections.emptySet());
    }
}
