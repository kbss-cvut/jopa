/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.sesame.config.RuntimeConfiguration;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.sesame.util.AxiomBuilder;
import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;

import java.util.*;
import java.util.stream.Collectors;

class AxiomLoader {

    private final Connector connector;
    private final ValueFactory valueFactory;

    private final Map<IRI, Assertion> propertyToAssertion;
    private Map<IRI, Assertion> explicitAssertions;
    private Map<IRI, Assertion> inferredAssertions;

    private final RuntimeConfiguration config;

    AxiomLoader(Connector connector, ValueFactory valueFactory, RuntimeConfiguration config) {
        this.connector = connector;
        this.valueFactory = valueFactory;
        this.propertyToAssertion = new HashMap<>();
        this.config = config;
    }

    Collection<Axiom<?>> loadAxioms(AxiomDescriptor axiomDescriptor) throws SesameDriverException {
        return findStatements(axiomDescriptor);
    }

    private Collection<Axiom<?>> findStatements(AxiomDescriptor descriptor) throws SesameDriverException {
        final Collection<Axiom<?>> result = new HashSet<>();
        final Resource subject = SesameUtils.toSesameIri(descriptor.getSubject().getIdentifier(), valueFactory);
        final Assertion unspecified = processAssertions(descriptor);
        final AxiomBuilder axiomBuilder = new AxiomBuilder(descriptor.getSubject(), propertyToAssertion, unspecified);
        final StatementLoader statementLoader = new StatementLoader(config, descriptor, connector, subject,
                axiomBuilder);
        if (unspecified == null || !unspecified.isInferred()) {
            statementLoader.setIncludeInferred(false);
            result.addAll(statementLoader.loadAxioms(explicitAssertions));
        }
        statementLoader.setIncludeInferred(true);
        result.addAll(statementLoader.loadAxioms(inferredAssertions));
        return result;
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
            final IRI property = SesameUtils.toSesameIri(a.getIdentifier(), valueFactory);
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
        final IRI sesameContext = SesameUtils.toSesameIri(context, valueFactory);
        final IRI subject = SesameUtils.toSesameIri(individual.getIdentifier(), valueFactory);
        final AxiomBuilder axiomBuilder = new AxiomBuilder(individual, Collections.emptyMap(),
                Assertion.createUnspecifiedPropertyAssertion(includeInferred));
        final Collection<Statement> statements = connector
                .findStatements(subject, null, null, includeInferred, sesameContext);
        return statements.stream().map(axiomBuilder::statementToAxiom).collect(Collectors.toSet());
    }
}
