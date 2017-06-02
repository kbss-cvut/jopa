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

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.semanticweb.owlapi.model.*;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

class ExplicitAxiomLoader implements AxiomLoader {

    private static final Assertion UNSPECIFIED_ASSERTION = Assertion.createUnspecifiedPropertyAssertion(false);

    private final OWLOntology ontology;
    private final OWLDataFactory dataFactory;

    private final OwlapiAdapter adapter;
    private final AxiomAdapter axiomAdapter;
    private final String language;

    private Set<Assertion> assertions;
    private Set<URI> assertionUris;

    ExplicitAxiomLoader(OwlapiAdapter adapter, OntologySnapshot snapshot) {
        this.adapter = adapter;
        this.ontology = snapshot.getOntology();
        this.dataFactory = snapshot.getDataFactory();
        this.axiomAdapter = new AxiomAdapter(dataFactory, adapter.getLanguage());
        this.language = adapter.getLanguage();
    }

    @Override
    public Collection<Axiom<?>> loadAxioms(NamedResource subject, Set<Assertion> assertions) {
        this.assertions = assertions;
        this.assertionUris = assertions.stream().map(NamedResource::getIdentifier).collect(Collectors.toSet());
        final OWLNamedIndividual individual = OwlapiUtils.getIndividual(subject, dataFactory);
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        if (assertions.contains(Assertion.createClassAssertion(false))) {
            axioms.addAll(adapter.getTypesHandler().getTypes(subject, null, false));
        }
        // This involves a lot of filtering, perhaps we should use EntitySearcher and look for values of concrete properties
        final Collection<OWLDataPropertyAssertionAxiom> dpAssertions = ontology.getDataPropertyAssertionAxioms(
                individual);
        axioms.addAll(dataPropertyValuesToAxioms(subject, dpAssertions));
        final Collection<OWLObjectPropertyAssertionAxiom> opAssertions = ontology.getObjectPropertyAssertionAxioms(
                individual);
        axioms.addAll(objectPropertyValuesToAxioms(subject, opAssertions));
        final Collection<OWLAnnotationAssertionAxiom> apAssertions = ontology.getAnnotationAssertionAxioms(
                individual.getIRI());
        axioms.addAll(annotationPropertyValuesToAxioms(subject, apAssertions));
        return axioms;
    }

    private Collection<Axiom<?>> dataPropertyValuesToAxioms(NamedResource subject,
                                                            Collection<OWLDataPropertyAssertionAxiom> axioms) {
        return axioms.stream().filter(axiom -> {
            final OWLLiteral value = axiom.getObject();
            final boolean propertyExists = doesPropertyExist(axiom.getProperty().asOWLDataProperty().getIRI());
            return propertyExists && OwlapiUtils.doesLanguageMatch(value, language);
        }).map(axiom -> axiomAdapter.toAxiom(subject, axiom, false))
                     .collect(Collectors.toList());
    }

    private boolean doesPropertyExist(IRI o) {
        return assertions.contains(UNSPECIFIED_ASSERTION) || assertionUris.contains(o.toURI());
    }

    private Collection<Axiom<?>> objectPropertyValuesToAxioms(NamedResource subject,
                                                              Collection<OWLObjectPropertyAssertionAxiom> axioms) {
        return axioms.stream().filter(axiom ->
                doesPropertyExist(axiom.getProperty().asOWLObjectProperty().getIRI()))
                     .map(axiom -> axiomAdapter.toAxiom(subject, axiom, false))
                     .collect(Collectors.toList());
    }

    private Collection<Axiom<?>> annotationPropertyValuesToAxioms(NamedResource subject,
                                                                  Collection<OWLAnnotationAssertionAxiom> axioms) {
        return axioms.stream().filter(axiom -> {
            final OWLAnnotationValue value = axiom.getValue();
            final boolean propertyExists = doesPropertyExist(axiom.getProperty().asOWLAnnotationProperty().getIRI());
            return propertyExists && (!value.asLiteral().isPresent() ||
                    OwlapiUtils.doesLanguageMatch(value.asLiteral().get(), language));
        }).map(axiom -> axiomAdapter.toAxiom(subject, axiom, false))
                     .collect(Collectors.toList());
    }

    @Override
    public Collection<Axiom<?>> loadPropertyAxioms(NamedResource subject) {
        final OWLNamedIndividual individual = OwlapiUtils.getIndividual(subject, dataFactory);
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        ontology.getDataPropertyAssertionAxioms(individual)
                .forEach(assertion -> axioms.add(axiomAdapter.toAxiom(subject, assertion, false)));
        ontology.getObjectPropertyAssertionAxioms(individual)
                .forEach(assertion -> axioms.add(axiomAdapter.toAxiom(subject, assertion, false)));
        ontology.getAnnotationAssertionAxioms(individual.getIRI())
                .forEach(assertion -> axioms.add(axiomAdapter.toAxiom(subject, assertion, false)));
        return axioms;
    }
}
