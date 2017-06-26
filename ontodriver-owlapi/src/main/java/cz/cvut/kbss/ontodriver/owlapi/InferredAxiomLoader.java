/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.exception.ReasonerNotAvailableException;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import cz.cvut.kbss.ontodriver.model.*;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.*;
import java.util.stream.Collectors;

public class InferredAxiomLoader implements AxiomLoader {

    private final OWLReasoner reasoner;
    private final OWLOntology ontology;
    private final OWLDataFactory dataFactory;

    private final OwlapiAdapter adapter;
    private final AxiomAdapter axiomAdapter;

    private NamedResource subject;

    InferredAxiomLoader(OwlapiAdapter adapter, OntologySnapshot snapshot) {
        this.adapter = adapter;
        this.reasoner = snapshot.getReasoner();
        this.ontology = snapshot.getOntology();
        this.dataFactory = snapshot.getDataFactory();
        this.axiomAdapter = new AxiomAdapter(snapshot.getDataFactory(), adapter.getLanguage());
    }

    @Override
    public Collection<Axiom<?>> loadAxioms(NamedResource subject, Set<Assertion> assertions) {
        this.subject = subject;
        if (assertions.isEmpty()) {
            return Collections.emptySet();
        }
        if (reasoner == null) {
            throw new ReasonerNotAvailableException();
        }
        reasoner.flush();
        final OWLNamedIndividual individual = OwlapiUtils.getIndividual(subject, dataFactory);
        final Collection<Axiom<?>> axioms = new HashSet<>();
        for (Assertion a : assertions) {
            switch (a.getType()) {
                case CLASS:
                    axioms.addAll(adapter.getTypesHandler().getTypes(subject, null, true));
                    break;
                case DATA_PROPERTY:
                    axioms.addAll(inferDataPropertyValues(individual, a));
                    break;
                case OBJECT_PROPERTY:
                    axioms.addAll(inferObjectPropertyValues(individual, a));
                    break;
                case PROPERTY:
                    // When we don't know, try all
                    axioms.addAll(adapter.getTypesHandler().getTypes(subject, null, true));
                    axioms.addAll(inferDataPropertyValues(individual, a));
                    axioms.addAll(inferObjectPropertyValues(individual, a));
                    break;
                default:
                    break;
            }
        }
        return axioms;
    }

    private Collection<Axiom<?>> inferDataPropertyValues(OWLNamedIndividual individual, Assertion dpAssertion) {
        final Set<OWLLiteral> literals = reasoner.getDataPropertyValues(individual, dataProperty(dpAssertion));
        final String lang = dpAssertion.hasLanguage() ? dpAssertion.getLanguage() : adapter.getLanguage();
        return literals.stream().filter(lit -> OwlapiUtils.doesLanguageMatch(lit, lang))
                       .map(owlLiteral -> new AxiomImpl<>(subject, dpAssertion,
                               new Value<>(OwlapiUtils.owlLiteralToValue(owlLiteral)))).collect(Collectors.toSet());
    }

    private OWLDataProperty dataProperty(Assertion dataPropertyAssertion) {
        return dataFactory.getOWLDataProperty(IRI.create(dataPropertyAssertion.getIdentifier()));
    }

    private Collection<Axiom<?>> inferObjectPropertyValues(OWLNamedIndividual individual, Assertion opAssertion) {
        final Set<OWLNamedIndividual> individuals = reasoner.getObjectPropertyValues(individual,
                objectProperty(opAssertion)).getFlattened();
        return individuals.stream().map(
                target -> axiomAdapter.createAxiom(subject, opAssertion, NamedResource.create(target.getIRI().toURI())))
                          .collect(
                                  Collectors.toList());
    }

    private OWLObjectProperty objectProperty(Assertion objectPropertyAssertion) {
        return dataFactory.getOWLObjectProperty(IRI.create(objectPropertyAssertion.getIdentifier()));
    }

    @Override
    public Collection<Axiom<?>> loadPropertyAxioms(NamedResource subject) {
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        final OWLNamedIndividual individual = OwlapiUtils.getIndividual(subject, dataFactory);
        for (OWLDataProperty dp : ontology.getDataPropertiesInSignature()) {
            final Set<OWLLiteral> values = reasoner.getDataPropertyValues(individual, dp);
            for (OWLLiteral literal : values) {
                axioms.add(axiomAdapter.createAxiom(subject,
                        Assertion.createDataPropertyAssertion(dp.getIRI().toURI(), true), literal));
            }
        }
        for (OWLObjectProperty op : ontology.getObjectPropertiesInSignature()) {
            final Set<OWLNamedIndividual> values = reasoner.getObjectPropertyValues(individual, op).getFlattened();
            for (OWLNamedIndividual ind : values) {
                axioms.add(axiomAdapter.createAxiom(subject,
                        Assertion.createObjectPropertyAssertion(op.getIRI().toURI(), true), NamedResource.create(
                                ind.getIRI().toURI())));
            }
        }
        return axioms;
    }
}
