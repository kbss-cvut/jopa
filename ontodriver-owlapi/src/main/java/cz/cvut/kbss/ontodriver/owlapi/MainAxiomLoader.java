/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

class MainAxiomLoader {

    private final OwlapiAdapter adapter;
    private final OntologySnapshot snapshot;

    private final OWLOntology ontology;

    private final Set<URI> inferredAssertionUris = new HashSet<>();
    private final Set<Assertion> inferredAssertions = new HashSet<>();
    private final Set<Assertion> explicitAssertions = new HashSet<>();

    MainAxiomLoader(OwlapiAdapter adapter, OntologySnapshot snapshot) {
        this.adapter = adapter;
        this.snapshot = snapshot;
        this.ontology = snapshot.getOntology();
    }

    Collection<Axiom<?>> findAxioms(AxiomDescriptor descriptor) {
        final NamedResource subject = descriptor.getSubject();
        if (!ontology.containsIndividualInSignature(IRI.create(subject.getIdentifier()))) {
            return Collections.emptySet();
        }
        resolveInferredAssertions(descriptor);

        final Collection<Axiom<?>> result = new ArrayList<>(loadInferredValues(subject));
        result.addAll(loadExplicitValues(subject));
        return result;
    }

    private void resolveInferredAssertions(AxiomDescriptor descriptor) {
        descriptor.getAssertions().forEach(assertion -> {
            if (assertion.isInferred()) {
                inferredAssertionUris.add(assertion.getIdentifier());
                inferredAssertions.add(assertion);
            } else {
                explicitAssertions.add(assertion);
            }
        });
    }

    private Collection<Axiom<?>> loadInferredValues(NamedResource subject) {
        if (inferredAssertions.isEmpty()) {
            return Collections.emptySet();
        }
        return new InferredAxiomLoader(adapter, snapshot).loadAxioms(subject, inferredAssertions);
    }

    private Collection<Axiom<?>> loadExplicitValues(NamedResource subject) {
        if (explicitAssertions.isEmpty()) {
            return Collections.emptySet();
        }
        final Collection<Axiom<?>> values = new ExplicitAxiomLoader(adapter, snapshot)
                .loadAxioms(subject, explicitAssertions);
        return values.stream().filter(axiom -> !inferredAssertionUris.contains(axiom.getAssertion().getIdentifier()))
                     .collect(Collectors.toList());
    }
}
