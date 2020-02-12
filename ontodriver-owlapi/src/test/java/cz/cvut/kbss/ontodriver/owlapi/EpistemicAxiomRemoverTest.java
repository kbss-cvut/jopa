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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

class EpistemicAxiomRemoverTest {

    private static final NamedResource SUBJECT = NamedResource.create("http://krizik.felk.cvut.cz/jopa#Individual");

    private OWLOntology ontology;

    private OWLOntologyManager manager;

    private OWLDataFactory dataFactory;

    @Mock
    private OwlapiAdapter adapterMock;

    private OWLNamedIndividual individual;
    private AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);

    private EpistemicAxiomRemover axiomRemover;

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final OntologySnapshot snapshot = TestUtils.initRealOntology(null);
        this.ontology = spy(snapshot.getOntology());
        this.manager = spy(snapshot.getOntologyManager());
        this.dataFactory = snapshot.getDataFactory();
        final OntologySnapshot snapshotToUse = new OntologySnapshot(ontology, manager, dataFactory, null);
        this.axiomRemover = new EpistemicAxiomRemover(adapterMock, snapshotToUse);
        this.individual = dataFactory.getOWLNamedIndividual(IRI.create(SUBJECT.getIdentifier()));
    }

    @Test
    void removeDoesNothingWhenNoMatchingValuesAreFound() {
        final Assertion clsAssertion = Assertion.createClassAssertion(false);
        descriptor.addAssertion(clsAssertion);
        axiomRemover.remove(descriptor);
        verify(adapterMock, never()).addTransactionalChanges(anyList());
    }

    @Test
    void removeAxiomsByDescriptorWithClassAssertions() {
        final Assertion clsAssertion = Assertion.createClassAssertion(false);
        descriptor.addAssertion(clsAssertion);
        initClassAssertions();
        axiomRemover.remove(descriptor);
        assertTrue(ontology.classAssertionAxioms(individual).collect(Collectors.toSet()).isEmpty());
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(adapterMock).addTransactionalChanges(captor.capture());
        final List<?> changes = captor.getValue();
        for (Object change : changes) {
            assertTrue(change instanceof RemoveAxiom);
            final RemoveAxiom ax = (RemoveAxiom) change;
            assertTrue(ax.getAxiom() instanceof OWLClassAssertionAxiom);
        }
    }

    private void initClassAssertions() {
        final OWLClass clsOne = dataFactory.getOWLClass(IRI.create("http://krizik.felk.cvut.cz/jopa#OWLClassA"));
        final OWLClass clsTwo = dataFactory.getOWLClass(IRI.create("http://krizik.felk.cvut.cz/jopa#OWLClassB"));
        manager.addAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(clsOne, individual));
        manager.addAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(clsTwo, individual));
    }

    @Test
    void removeAxiomsByDescriptorWithDataPropertyAssertions() {
        final Assertion dpAssertion = Assertion
                .createDataPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#dataProperty"), false);
        final OWLDataProperty odp = dataFactory.getOWLDataProperty(IRI.create(dpAssertion.getIdentifier()));
        descriptor.addAssertion(dpAssertion);
        initDataPropertyAssertions(odp);
        final int count =
                EntitySearcher.getDataPropertyValues(individual, odp, ontology).collect(Collectors.toSet()).size();
        axiomRemover.remove(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(adapterMock).addTransactionalChanges(captor.capture());
        final List<?> changes = captor.getValue();
        assertEquals(count, changes.size());
        verifyRemoveAxioms(odp, OWLDataPropertyAssertionAxiom.class, changes);
    }

    private void initDataPropertyAssertions(OWLDataProperty... dataProperties) {
        for (OWLDataProperty dp : dataProperties) {
            manager.addAxiom(ontology, dataFactory.getOWLDataPropertyAssertionAxiom(dp, individual, 117));
            manager.addAxiom(ontology, dataFactory.getOWLDataPropertyAssertionAxiom(dp, individual, 87));
        }
    }

    private void verifyRemoveAxioms(OWLProperty property, Class<? extends OWLPropertyAssertionAxiom> assertionType,
                                    Collection<?> changes) {
        for (Object change : changes) {
            assertTrue(change instanceof RemoveAxiom);
            final RemoveAxiom ax = (RemoveAxiom) change;
            assertTrue(assertionType.isAssignableFrom(ax.getAxiom().getClass()));
            final OWLPropertyAssertionAxiom removed = (OWLPropertyAssertionAxiom) ax.getAxiom();
            assertEquals(property, removed.getProperty());
        }
    }

    @Test
    void removeAxiomsByDescriptorWithObjectPropertyAssertions() {
        final Assertion opAssertion = Assertion
                .createObjectPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#objectProperty"), false);
        final OWLObjectProperty oop = dataFactory.getOWLObjectProperty(IRI.create(opAssertion.getIdentifier()));
        descriptor.addAssertion(opAssertion);
        initObjectPropertyAssertions(oop);
        final int count =
                EntitySearcher.getObjectPropertyValues(individual, oop, ontology).collect(Collectors.toSet()).size();
        axiomRemover.remove(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(adapterMock).addTransactionalChanges(captor.capture());
        final List<?> changes = captor.getValue();
        assertEquals(count, changes.size());
        verifyRemoveAxioms(oop, OWLObjectPropertyAssertionAxiom.class, changes);
    }

    private void initObjectPropertyAssertions(OWLObjectProperty... objectProperties) {
        final OWLNamedIndividual value = dataFactory
                .getOWLNamedIndividual(IRI.create("http://krizik.felk.cvut.cz/jopa#IndividualTwo"));
        for (OWLObjectProperty op : objectProperties) {
            manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(op, individual, value));
        }
    }

    @Test
    void removeRetainsValuesOfAssertionsNotPresentInDescriptor() {
        final Assertion opAssertion = Assertion
                .createObjectPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#objectProperty"), false);
        final Assertion opAssertionTwo = Assertion
                .createObjectPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#objectPropertyTwo"), false);
        descriptor.addAssertion(opAssertion);
        final OWLObjectProperty opOne = dataFactory.getOWLObjectProperty(IRI.create(opAssertion.getIdentifier()));
        final OWLObjectProperty opTwo = dataFactory.getOWLObjectProperty(IRI.create(opAssertionTwo.getIdentifier()));
        initObjectPropertyAssertions(opOne, opTwo);
        final int countTwo =
                EntitySearcher.getObjectPropertyValues(individual, opTwo, ontology).collect(Collectors.toSet()).size();
        axiomRemover.remove(descriptor);
        final int resultCountOne =
                EntitySearcher.getObjectPropertyValues(individual, opOne, ontology).collect(Collectors.toSet()).size();
        final int resultCountTwo =
                EntitySearcher.getObjectPropertyValues(individual, opTwo, ontology).collect(Collectors.toSet()).size();
        assertEquals(0, resultCountOne);
        assertEquals(countTwo, resultCountTwo);
    }

    @Test
    void removeAxiomsByDescriptorWithAnnotationPropertyAssertions() {
        final Assertion apAssertion = Assertion.createAnnotationPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/jopa#annotationProperty"), false);
        final OWLAnnotationProperty oap = dataFactory.getOWLAnnotationProperty(IRI.create(apAssertion.getIdentifier()));
        descriptor.addAssertion(apAssertion);
        initAnnotationAssertions(oap);
        axiomRemover.remove(descriptor);
        assertEquals(0,
                EntitySearcher.getAnnotationAssertionAxioms(individual.getIRI(), ontology).collect(Collectors.toSet())
                              .size());
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(adapterMock).addTransactionalChanges(captor.capture());
        final List<?> changes = captor.getValue();
        verifyAnnotationRemoveAxioms(oap, changes);
    }

    private void initAnnotationAssertions(OWLAnnotationProperty... annotationProperties) {
        for (OWLAnnotationProperty ap : annotationProperties) {
            manager.addAxiom(ontology, dataFactory
                    .getOWLAnnotationAssertionAxiom(ap, individual.getIRI(), dataFactory.getOWLLiteral(117)));
            manager.addAxiom(ontology, dataFactory
                    .getOWLAnnotationAssertionAxiom(ap, individual.getIRI(), dataFactory.getOWLLiteral("String")));
        }
    }

    private void verifyAnnotationRemoveAxioms(OWLProperty property, Collection<?> changes) {
        for (Object change : changes) {
            assertTrue(change instanceof RemoveAxiom);
            final RemoveAxiom ax = (RemoveAxiom) change;
            assertTrue(OWLAnnotationAssertionAxiom.class.isAssignableFrom(ax.getAxiom().getClass()));
            final OWLAnnotationAssertionAxiom removed = (OWLAnnotationAssertionAxiom) ax.getAxiom();
            assertEquals(property, removed.getProperty());
        }
    }

    @Test
    void removeCombinationOfDataObjectAndAnnotationPropertiesAndClassAssertion() {
        final Set<OWLPropertyExpression> removed = initForCombinedTest();
        axiomRemover.remove(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(adapterMock).addTransactionalChanges(captor.capture());
        final List<?> changes = captor.getValue();
        for (Object change : changes) {
            final RemoveAxiom ax = (RemoveAxiom) change;
            final OWLPropertyExpression op;
            if (!(ax.getAxiom() instanceof OWLClassAssertionAxiom)) {
                if (ax.getAxiom() instanceof OWLAnnotationAssertionAxiom) {
                    op = ((OWLAnnotationAssertionAxiom) ax.getAxiom()).getProperty();
                } else {
                    op = ((OWLPropertyAssertionAxiom) ax.getAxiom()).getProperty();
                }
                assertTrue(removed.contains(op));
            }
        }
    }

    private Set<OWLPropertyExpression> initForCombinedTest() {
        final Assertion clsAssertion = Assertion.createClassAssertion(false);
        descriptor.addAssertion(clsAssertion);
        initClassAssertions();
        final Assertion dpAssertion = Assertion
                .createDataPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#dataProperty"), false);
        final OWLDataProperty odp = dataFactory.getOWLDataProperty(IRI.create(dpAssertion.getIdentifier()));
        descriptor.addAssertion(dpAssertion);
        initDataPropertyAssertions(odp);
        final Assertion opAssertion = Assertion
                .createObjectPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#objectProperty"), false);
        final Assertion opAssertionTwo = Assertion
                .createObjectPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#objectPropertyTwo"), false);
        descriptor.addAssertion(opAssertion);
        final OWLObjectProperty opOne = dataFactory.getOWLObjectProperty(IRI.create(opAssertion.getIdentifier()));
        final OWLObjectProperty opTwo = dataFactory.getOWLObjectProperty(IRI.create(opAssertionTwo.getIdentifier()));
        initObjectPropertyAssertions(opOne, opTwo);
        final Assertion ap = Assertion.createAnnotationPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/jopa#annotationProperty"), false);
        final OWLAnnotationProperty oap = dataFactory.getOWLAnnotationProperty(IRI.create(ap.getIdentifier()));
        descriptor.addAssertion(ap);
        initAnnotationAssertions(oap);
        return new HashSet<>(Arrays.asList(odp, opOne, oap));
    }

    @Test
    void removeAxiomsRemovesCorrespondingAssertions() {
        final Map<Assertion, Set<Value<?>>> toRemove = initForRemoveAxiomsTest();
        axiomRemover.removeAxioms(SUBJECT, toRemove);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(adapterMock).addTransactionalChanges(captor.capture());
        final List<?> changes = captor.getValue();
        final int toRemoveSize = toRemove.values().stream().mapToInt(Set::size).sum();
        assertEquals(toRemoveSize, changes.size());
        for (Object change : changes) {
            final RemoveAxiom ax = (RemoveAxiom) change;
            final Assertion a;
            if (ax.getAxiom().annotationPropertiesInSignature().anyMatch(x -> true)) {
                a = Assertion.createPropertyAssertion(
                        ax.getAxiom().annotationPropertiesInSignature().iterator().next().getIRI().toURI(), false);
            } else if (ax.getAxiom().dataPropertiesInSignature().anyMatch(x -> true)) {
                a = Assertion.createPropertyAssertion(
                        ax.getAxiom().dataPropertiesInSignature().iterator().next().getIRI().toURI(), false);
            } else if (ax.getAxiom().objectPropertiesInSignature().anyMatch(x -> true)) {
                a = Assertion.createPropertyAssertion(
                        ax.getAxiom().objectPropertiesInSignature().iterator().next().getIRI().toURI(), false);
            } else {
                a = Assertion.createPropertyAssertion(URI.create(Vocabulary.RDF_TYPE), false);
            }
            assertTrue(toRemove.containsKey(a));
        }
    }

    private Map<Assertion, Set<Value<?>>> initForRemoveAxiomsTest() {
        final int valueCount = 5;
        final Map<Assertion, Set<Value<?>>> map = new HashMap<>();
        final Assertion op = Assertion
                .createPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#objectProperty"), false);
        final OWLObjectProperty owlOp = dataFactory.getOWLObjectProperty(IRI.create(op.getIdentifier()));
        map.put(op, new HashSet<>());
        for (int i = 0; i < valueCount; i++) {
            final String strVal = "http://krizik.felk.cvut.cz/jopa#Individual-" + i;
            if (i % 2 == 0) {
                map.get(op).add(new Value<>(strVal));
            }
            manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(owlOp, individual,
                    dataFactory.getOWLNamedIndividual(IRI.create(strVal))));
        }
        final Assertion dp = Assertion
                .createPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#dataProperty"), false);
        final OWLDataProperty owlDp = dataFactory.getOWLDataProperty(IRI.create(dp.getIdentifier()));
        map.put(dp, new HashSet<>());
        for (int i = 0; i < valueCount; i++) {
            final String strVal = "John117";
            if (i % 2 == 0) {
                map.get(dp).add(new Value<>(strVal));
            }
            manager.addAxiom(ontology, dataFactory.getOWLDataPropertyAssertionAxiom(owlDp, individual, strVal));
        }
        final Assertion ap = Assertion
                .createPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#annotationProperty"), false);
        final OWLAnnotationProperty owlAp = dataFactory.getOWLAnnotationProperty(IRI.create(ap.getIdentifier()));
        map.put(ap, new HashSet<>());
        for (int i = 0; i < valueCount; i++) {
            final String strVal = "John117";
            if (i % 2 == 0) {
                map.get(ap).add(new Value<>(strVal));
            }
            manager.addAxiom(ontology, dataFactory
                    .getOWLAnnotationAssertionAxiom(owlAp, individual.getIRI(), dataFactory.getOWLLiteral(strVal)));
        }
        final Assertion ca = Assertion.createPropertyAssertion(URI.create(Vocabulary.RDF_TYPE), false);
        map.put(ca, new HashSet<>());
        for (int i = 0; i < valueCount; i++) {
            final String strVal = "http://krizik.felk.cvut.cz/jopa#OWLClass" + i;
            if (i % 2 == 0) {
                map.get(ca).add(new Value<>(strVal));
            }
            manager.addAxiom(ontology,
                    dataFactory.getOWLClassAssertionAxiom(dataFactory.getOWLClass(IRI.create(strVal)), individual));
        }
        return map;
    }

    @Test
    void removeAxiomsWithUnknownPropertyDoesNothing() {
        final Map<Assertion, Set<Value<?>>> toRemove = new HashMap<>();
        final Assertion unknownProperty = Assertion
                .createPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#unknownProperty"), false);
        toRemove.put(unknownProperty, Collections.singleton(new Value<>("StringValue")));

        axiomRemover.removeAxioms(SUBJECT, toRemove);
        verify(manager, never()).applyChange(any(OWLOntologyChange.class));
        verify(manager).applyChanges(Collections.emptyList());
    }
}