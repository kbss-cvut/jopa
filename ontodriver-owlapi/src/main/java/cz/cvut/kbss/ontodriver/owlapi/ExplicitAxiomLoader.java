/**
 * Copyright (C) 2019 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.semanticweb.owlapi.model.*;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
        // This involves a lot of filtering, perhaps we should use EntitySearcher and look for values of concrete properties
        final Stream<OWLDataPropertyAssertionAxiom> dpAssertions = ontology.dataPropertyAssertionAxioms(individual);
        axioms.addAll(dataPropertyValuesToAxioms(subject, dpAssertions));
        final Stream<OWLObjectPropertyAssertionAxiom> opAssertions = ontology.objectPropertyAssertionAxioms(individual);
        axioms.addAll(objectPropertyValuesToAxioms(subject, opAssertions));
        final Stream<OWLAnnotationAssertionAxiom> apAssertions = ontology
                .annotationAssertionAxioms(individual.getIRI());
        axioms.addAll(annotationPropertyValuesToAxioms(subject, apAssertions));
        return axioms;
    }

    private Collection<Axiom<?>> dataPropertyValuesToAxioms(NamedResource subject,
                                                            Stream<OWLDataPropertyAssertionAxiom> axioms) {
        return axioms.filter(this::shouldLoadDataPropertyValue)
                     .map(axiom -> axiomAdapter.toAxiom(subject, axiom, false))
                     .collect(Collectors.toList());
    }

    private boolean shouldLoadDataPropertyValue(OWLDataPropertyAssertionAxiom axiom) {
        final OWLLiteral value = axiom.getObject();
        final IRI dpIri = axiom.getProperty().asOWLDataProperty().getIRI();
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

    private Collection<Axiom<?>> objectPropertyValuesToAxioms(NamedResource subject,
                                                              Stream<OWLObjectPropertyAssertionAxiom> axiomStream) {
        return axiomStream.filter(axiom ->
                doesPropertyExist(axiom.getProperty().asOWLObjectProperty().getIRI()))
                          .map(axiom -> axiomAdapter.toAxiom(subject, axiom, false))
                          .collect(Collectors.toList());
    }

    private Collection<Axiom<?>> annotationPropertyValuesToAxioms(NamedResource subject,
                                                                  Stream<OWLAnnotationAssertionAxiom> axioms) {
        return axioms.filter(this::shouldLoadAnnotationPropertyValue)
                     .map(axiom -> axiomAdapter.toAxiom(subject, axiom, false))
                     .collect(Collectors.toList());
    }

    private boolean shouldLoadAnnotationPropertyValue(OWLAnnotationAssertionAxiom axiom) {
        final OWLAnnotationValue value = axiom.getValue();
        final IRI apIri = axiom.getProperty().asOWLAnnotationProperty().getIRI();
        final Optional<Assertion> assertion = assertionForAxiom(apIri);
        return assertion.isPresent() && (!value.asLiteral().isPresent() ||
                OwlapiUtils.doesLanguageMatch(value.asLiteral().get(), assertion.get()));
    }

    @Override
    public Collection<Axiom<?>> loadPropertyAxioms(NamedResource subject) {
        final OWLNamedIndividual individual = OwlapiUtils.getIndividual(subject, dataFactory);
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        ontology.dataPropertyAssertionAxioms(individual)
                .forEach(assertion -> axioms.add(axiomAdapter.toAxiom(subject, assertion, false)));
        ontology.objectPropertyAssertionAxioms(individual)
                .forEach(assertion -> axioms.add(axiomAdapter.toAxiom(subject, assertion, false)));
        ontology.annotationAssertionAxioms(individual.getIRI())
                .forEach(assertion -> axioms.add(axiomAdapter.toAxiom(subject, assertion, false)));
        return axioms;
    }
}
