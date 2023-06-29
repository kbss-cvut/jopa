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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

class ExplicitAxiomLoader implements AxiomLoader {

    private static final Assertion UNSPECIFIED_ASSERTION = Assertion.createUnspecifiedPropertyAssertion(false);

    private final OWLOntology ontology;
    private final OWLDataFactory dataFactory;

    private final OwlapiAdapter adapter;
    private final AxiomAdapter axiomAdapter;

    private Map<URI, Assertion> assertionMap;

    ExplicitAxiomLoader(OwlapiAdapter adapter, OntologySnapshot snapshot) {
        this.adapter = adapter;
        this.ontology = snapshot.getOntology();
        this.dataFactory = snapshot.getDataFactory();
        this.axiomAdapter = new AxiomAdapter(dataFactory);
    }

    @Override
    public Collection<Axiom<?>> loadAxioms(NamedResource subject, Set<Assertion> assertions) {
        this.assertionMap = new HashMap<>(assertions.size());
        assertions.forEach(a -> assertionMap.put(a.getIdentifier(), a));
        final OWLNamedIndividual individual = OwlapiUtils.getIndividual(subject, dataFactory);
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        if (assertions.contains(Assertion.createClassAssertion(false))) {
            axioms.addAll(adapter.getTypesHandler().getTypes(subject, null, false));
        }
        axioms.addAll(loadDataPropertyAxioms(individual, subject, false));
        axioms.addAll(loadObjectPropertyAxioms(individual, subject, false));
        axioms.addAll(loadAnnotationPropertyAxioms(individual, subject, false));
        return axioms;
    }

    private Collection<Axiom<?>> loadDataPropertyAxioms(OWLNamedIndividual individual, NamedResource subject,
                                                        boolean loadAll) {
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        EntitySearcher.getDataPropertyValues(individual, ontology.importsClosure()).forEach((dp, value) -> {
            if (loadAll || shouldLoadDataPropertyValue(dp, value)) {
                axioms.add(axiomAdapter.toAxiom(subject, dp, value));
            }
        });
        return axioms;
    }

    private boolean shouldLoadDataPropertyValue(OWLDataPropertyExpression dp, OWLLiteral value) {
        final IRI dpIri = dp.asOWLDataProperty().getIRI();
        final Optional<Assertion> assertion = assertionForAxiom(dpIri);
        return assertion.isPresent() && OwlapiUtils.doesLanguageMatch(value, assertion.get());
    }

    private Optional<Assertion> assertionForAxiom(IRI propertyIri) {
        if (!doesPropertyExist(propertyIri)) {
            return Optional.empty();
        }
        final URI dpUri = propertyIri.toURI();
        // Note: I don't really like the fact that we are basing this on a randomly generated identifier of the unspecified
        // property. Perhaps the strategy of using unspecified properties should be revisited.
        return Optional.of(assertionMap.containsKey(dpUri) ? assertionMap.get(dpUri) :
                assertionMap.get(UNSPECIFIED_ASSERTION.getIdentifier()));
    }

    private boolean doesPropertyExist(IRI o) {
        return assertionMap.containsKey(o.toURI()) || assertionMap.containsKey(UNSPECIFIED_ASSERTION.getIdentifier());
    }

    private Collection<Axiom<?>> loadObjectPropertyAxioms(OWLNamedIndividual individual, NamedResource subject, boolean loadAll) {
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        EntitySearcher.getObjectPropertyValues(individual, ontology.importsClosure())
                      .forEach((op, value) -> {
                          if (loadAll || doesPropertyExist(op.getNamedProperty().getIRI())) {
                              axioms.add(axiomAdapter.toAxiom(subject, op, value));
                          }
                      });
        return axioms;
    }

    private Collection<Axiom<?>> loadAnnotationPropertyAxioms(OWLNamedIndividual individual, NamedResource subject,
                                                              boolean loadAll) {
        return ontology.importsClosure().flatMap(
                onto -> EntitySearcher.getAnnotationAssertionAxioms(individual.getIRI(), onto)
                                      .filter(a -> loadAll || shouldLoadAnnotationPropertyValue(a)))
                       .map(axiom -> axiomAdapter.toAxiom(subject, axiom)).collect(
                        Collectors.toSet());
    }


    private boolean shouldLoadAnnotationPropertyValue(OWLAnnotationAssertionAxiom axiom) {
        final OWLAnnotationValue value = axiom.getValue();
        final IRI apIri = axiom.getProperty().asOWLAnnotationProperty().getIRI();
        final Optional<Assertion> assertion = assertionForAxiom(apIri);
        return assertion.isPresent() && (value.asLiteral().isEmpty() ||
                OwlapiUtils.doesLanguageMatch(value.asLiteral().get(), assertion.get()));
    }

    @Override
    public Collection<Axiom<?>> loadPropertyAxioms(NamedResource subject) {
        final OWLNamedIndividual individual = OwlapiUtils.getIndividual(subject, dataFactory);
        final Collection<Axiom<?>> axioms = new ArrayList<>(loadDataPropertyAxioms(individual, subject, true));
        axioms.addAll(loadObjectPropertyAxioms(individual, subject, true));
        axioms.addAll(loadAnnotationPropertyAxioms(individual, subject, true));
        return axioms;
    }
}
