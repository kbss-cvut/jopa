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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.descriptor.AbstractAxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.rdf4j.connector.RepoConnection;
import cz.cvut.kbss.ontodriver.rdf4j.connector.SubjectPredicateContext;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.ValueConverter;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils.toRdf4jIri;

/**
 * This class performs an epistemic remove of axioms described by the axiom descriptor.
 * <p/>
 * Epistemic remove means that only information known to the application is deleted. The assertions in the descriptor
 * represent this information. Thus, only these assertions are removed from the ontology. Note that if the descriptor
 * contains an unspecified property assertion, all property assertions related to the subject individual are removed
 * from the property's context.
 */
class EpistemicAxiomRemover {

    private final RepoConnection connector;
    private final ValueFactory valueFactory;

    EpistemicAxiomRemover(RepoConnection connector, ValueFactory valueFactory) {
        this.connector = connector;
        this.valueFactory = valueFactory;
    }

    void remove(AbstractAxiomDescriptor axiomDescriptor) throws Rdf4jDriverException {
        final Resource individual = toRdf4jIri(axiomDescriptor.getSubject(), valueFactory);
        final Collection<SubjectPredicateContext> toRemove = new HashSet<>();
        for (Assertion a : axiomDescriptor.getAssertions()) {
            final Set<Resource> contexts = axiomDescriptor.getAssertionContexts(a).stream()
                                                     .map(uri -> toRdf4jIri(uri, valueFactory))
                                                     .collect(Collectors.toSet());
            toRemove.add(new SubjectPredicateContext(individual, toRdf4jIri(a, valueFactory), contexts));
        }
        connector.removePropertyValues(toRemove);
    }

    void remove(NamedResource individual, Map<Assertion, Set<Value<?>>> values, java.net.URI context)
            throws Rdf4jDriverException {
        final IRI repoContext = toRdf4jIri(context, valueFactory);
        final Resource subject = toRdf4jIri(individual.getIdentifier(), valueFactory);
        final Collection<Statement> toRemove = new ArrayList<>();
        final ValueConverter valueConverter = new ValueConverter(valueFactory);
        for (Map.Entry<Assertion, Set<Value<?>>> entry : values.entrySet()) {
            final IRI property = toRdf4jIri(entry.getKey(), valueFactory);
            for (Value<?> val : entry.getValue()) {
                final org.eclipse.rdf4j.model.Value rdf4jValue = valueConverter.toRdf4jValue(entry.getKey(), val);
                if (repoContext != null) {
                    toRemove.add(valueFactory.createStatement(subject, property, rdf4jValue, repoContext));
                } else {
                    toRemove.add(valueFactory.createStatement(subject, property, rdf4jValue));
                }
            }
        }
        connector.removeStatements(toRemove);
    }
}
