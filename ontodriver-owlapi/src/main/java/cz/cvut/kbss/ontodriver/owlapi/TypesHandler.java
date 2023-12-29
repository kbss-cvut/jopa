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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.change.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.owlapi.change.MutableRemoveAxiom;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

class TypesHandler {

    private final OWLOntology ontology;
    private final OWLDataFactory dataFactory;
    private final OWLReasoner reasoner;
    private final OntologySnapshot snapshot;

    private final OwlapiAdapter adapter;

    TypesHandler(OwlapiAdapter adapter, OntologySnapshot snapshot) {
        this.adapter = adapter;
        this.snapshot = snapshot;
        this.ontology = snapshot.getOntology();
        this.dataFactory = snapshot.getDataFactory();
        this.reasoner = snapshot.getReasoner();
    }

    Set<Axiom<URI>> getTypes(NamedResource subject, Collection<URI> contexts, boolean includeInferred) {
        final Collection<? extends OWLClassExpression> owlClasses;
        if (!includeInferred) {
            owlClasses = loadExplicitClasses(subject);
        } else {
            owlClasses = inferClasses(subject);
        }
        return owlClassesToAxioms(subject, includeInferred, owlClasses);
    }

    private Collection<OWLClassExpression> loadExplicitClasses(NamedResource subject) {
        return EntitySearcher.getTypes(getIndividual(subject), ontology.importsClosure()).collect(Collectors.toSet());
    }

    private OWLNamedIndividual getIndividual(NamedResource subject) {
        return dataFactory.getOWLNamedIndividual(IRI.create(subject.getIdentifier()));
    }

    private static Set<Axiom<URI>> owlClassesToAxioms(NamedResource subject, boolean inferred,
                                                      Collection<? extends OWLClassExpression> owlClasses) {
        return owlClasses.stream().map(expr -> new AxiomImpl<>(subject,
                        Assertion.createClassAssertion(inferred), new Value<>(expr.asOWLClass().getIRI().toURI())))
                .collect(Collectors.toSet());
    }

    private Collection<? extends OWLClassExpression> inferClasses(NamedResource subject) {
        final OWLNamedIndividual individual = getIndividual(subject);
        return reasoner.getTypes(individual, false).entities().collect(Collectors.toSet());
    }

    void addTypes(NamedResource subject, URI context, Set<URI> types) {
        assert !types.isEmpty();

        final List<OWLAxiom> axioms = getOwlAxiomsForTypes(subject, types);
        final List<TransactionalChange> changes = axioms.stream().map(axiom -> new MutableAddAxiom(ontology, axiom))
                                                        .collect(Collectors.toList());

        adapter.addTransactionalChanges(snapshot.applyChanges(changes));
    }

    private List<OWLAxiom> getOwlAxiomsForTypes(NamedResource subject, Set<URI> types) {
        final List<OWLAxiom> axioms = new ArrayList<>(types.size());
        final OWLNamedIndividual individual = getIndividual(subject);
        axioms.addAll(types.stream().map(type -> dataFactory
                        .getOWLClassAssertionAxiom(dataFactory.getOWLClass(IRI.create(type)), individual))
                .collect(Collectors.toList()));
        return axioms;
    }

    void removeTypes(NamedResource subject, URI context, Set<URI> types) {
        assert !types.isEmpty();

        final List<OWLAxiom> axioms = getOwlAxiomsForTypes(subject, types);
        final List<TransactionalChange> changes = axioms.stream().map(axiom -> new MutableRemoveAxiom(ontology, axiom))
                                                        .collect(Collectors.toList());

        adapter.addTransactionalChanges(snapshot.applyChanges(changes));
    }
}
