/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNodeSet;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.net.URI;
import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public class PropertiesHandlerTest {

    private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#Entity");
    private static final NamedResource INDIVIDUAL = NamedResource.create(PK);

    private static final URI DP_ONE = URI.create("http://krizik.felk.cvut.cz/dpOne");
    private static final URI DP_TWO = URI.create("http://krizik.felk.cvut.cz/dpTwo");
    private static final URI OP_ONE = URI.create("http://krizik.felk.cvut.cz/opOne");

    @Mock
    private OwlapiAdapter adapterMock;

    @Mock
    private OWLOntology ontologyMock;
    @Mock
    private OWLOntologyManager managerMock;
    @Mock
    private OWLReasoner reasonerMock;

    private OWLDataFactory dataFactory;

    private PropertiesHandler propertiesHandler;


    private Set<Object> dataValues = new HashSet<>();
    private Set<Object> objectValues = new HashSet<>();

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.dataFactory = new OWLDataFactoryImpl();
        this.propertiesHandler = new PropertiesHandler(adapterMock,
                new OntologySnapshot(ontologyMock, managerMock, dataFactory, reasonerMock));
    }

    @Test
    public void getPropertiesLoadsExplicitPropertiesFromOntology() throws Exception {
        initSampleProperties();
        final Collection<Axiom<?>> axioms = propertiesHandler.getProperties(INDIVIDUAL, false);
        assertFalse(axioms.isEmpty());
        final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(IRI.create(PK));
        verify(ontologyMock).getDataPropertyAssertionAxioms(individual);
        verify(ontologyMock).getObjectPropertyAssertionAxioms(individual);
        verify(ontologyMock).getAnnotationAssertionAxioms(individual.getIRI());
        verifyAxioms(axioms, Assertion.AssertionType.DATA_PROPERTY, dataValues);
        verifyAxioms(axioms, Assertion.AssertionType.OBJECT_PROPERTY, objectValues);
    }

    private void initSampleProperties() {
        final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(IRI.create(PK));
        when(ontologyMock.getDataPropertyAssertionAxioms(individual)).thenReturn(initSampleDataProperties());
        when(ontologyMock.getObjectPropertyAssertionAxioms(individual)).thenReturn(initSampleObjectProperties());
    }

    private Set<OWLDataPropertyAssertionAxiom> initSampleDataProperties() {
        final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(IRI.create(PK));
        final OWLDataProperty dpOne = dataFactory.getOWLDataProperty(IRI.create(DP_ONE));
        final Set<OWLDataPropertyAssertionAxiom> dpValues = new HashSet<>();
        dpValues.add(dataFactory.getOWLDataPropertyAssertionAxiom(dpOne, individual, 100));
        dataValues.add(100);
        dpValues.add(dataFactory.getOWLDataPropertyAssertionAxiom(dpOne, individual, 1000));
        dataValues.add(1000);
        final OWLDataProperty dpTwo = dataFactory.getOWLDataProperty(IRI.create(DP_TWO));
        dpValues.add(dataFactory.getOWLDataPropertyAssertionAxiom(dpTwo, individual, "Test"));
        dataValues.add("Test");
        dpValues.add(dataFactory.getOWLDataPropertyAssertionAxiom(dpTwo, individual, "TestTwo"));
        dataValues.add("TestTwo");
        return dpValues;
    }

    private Set<OWLObjectPropertyAssertionAxiom> initSampleObjectProperties() {
        final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(IRI.create(PK));
        final OWLObjectProperty op = dataFactory.getOWLObjectProperty(IRI.create(OP_ONE));
        objectValues.add(INDIVIDUAL);
        final Set<OWLObjectPropertyAssertionAxiom> opValues = new HashSet<>();
        opValues.add(dataFactory.getOWLObjectPropertyAssertionAxiom(op, individual, individual));
        return opValues;
    }

    private void verifyAxioms(Collection<Axiom<?>> axioms, Assertion.AssertionType assertionType,
                              Set<?> expectedValues) {
        boolean found;
        for (Object val : expectedValues) {
            found = false;
            for (Axiom<?> ax : axioms) {
                if (ax.getValue().getValue().equals(val)) {
                    found = true;
                    assertEquals(assertionType, ax.getAssertion().getType());
                    break;
                }
            }
            assertTrue(found);
        }
    }

    @Test
    public void getPropertiesReturnsEmptyCollectionsForNonExistingIndividual() throws Exception {
        final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(IRI.create(PK));
        initEmptyOntology(individual);

        final Collection<Axiom<?>> axioms = propertiesHandler.getProperties(INDIVIDUAL, false);
        assertNotNull(axioms);
        assertTrue(axioms.isEmpty());
    }

    private void initEmptyOntology(OWLNamedIndividual individual) {
        when(ontologyMock.getDataPropertyAssertionAxioms(individual)).thenReturn(Collections.emptySet());
        when(ontologyMock.getObjectPropertyAssertionAxioms(individual)).thenReturn(Collections.emptySet());
        when(ontologyMock.getAnnotationAssertionAxioms(individual.getIRI())).thenReturn(Collections.emptySet());
    }

    @Test
    public void getPropertiesWithInferenceGetsInferredProperties() throws Exception {
        final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(IRI.create(PK));
        initEmptyOntology(individual);
        when(ontologyMock.getDataPropertiesInSignature()).thenReturn(new HashSet<>(
                Arrays.asList(dataFactory.getOWLDataProperty(IRI.create(DP_ONE)),
                        dataFactory.getOWLDataProperty(IRI.create(DP_TWO)))));
        when(ontologyMock.getObjectPropertiesInSignature())
                .thenReturn(Collections.singleton(dataFactory.getOWLObjectProperty(IRI.create(OP_ONE))));
        when(reasonerMock.getDataPropertyValues(eq(individual), any(OWLDataProperty.class)))
                .thenReturn(Collections.emptySet());
        when(reasonerMock.getObjectPropertyValues(eq(individual),
                any(OWLObjectProperty.class))).thenReturn(new OWLNamedIndividualNodeSet());

        final Collection<Axiom<?>> axioms = propertiesHandler.getProperties(INDIVIDUAL, true);

        assertNotNull(axioms);
        verify(reasonerMock, atLeastOnce()).getDataPropertyValues(eq(individual), any(OWLDataProperty.class));
        verify(reasonerMock, atLeastOnce()).getObjectPropertyValues(eq(individual), any(OWLObjectProperty.class));
    }

    @Test
    public void addPropertiesForExistingDataPropertyAddsAssertionsToOntology() throws Exception {
        final Assertion assertion = Assertion.createDataPropertyAssertion(DP_ONE, false);
        final Set<Value<?>> values = new HashSet<>(
                Arrays.asList(new Value<>("One"), new Value<>("Two"), new Value<>("Three")));
        when(ontologyMock.containsDataPropertyInSignature(IRI.create(DP_ONE))).thenReturn(true);

        propertiesHandler.addProperties(INDIVIDUAL, Collections.singletonMap(assertion, values));
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(managerMock).applyChanges(captor.capture());
        for (Object ob : captor.getValue()) {
            assertTrue(ob instanceof AddAxiom);
            final AddAxiom ax = (AddAxiom) ob;
            assertTrue(ax.getAxiom() instanceof OWLDataPropertyAssertionAxiom);
            final OWLDataPropertyAssertionAxiom dp = (OWLDataPropertyAssertionAxiom) ax.getAxiom();
            assertTrue(values.contains(new Value<>(OwlapiUtils.owlLiteralToValue(dp.getObject()))));
        }
    }

    @Test
    public void addPropertiesForExistingObjectPropertyAddsAssertionsToOntology() throws Exception {
        final Assertion assertion = Assertion.createObjectPropertyAssertion(OP_ONE, false);
        final Set<Value<?>> values = new HashSet<>(Arrays.asList(
                new Value<>(NamedResource.create("http://individualOne")),
                new Value<>(NamedResource.create("http://individualTwo"))));
        when(ontologyMock.containsObjectPropertyInSignature(IRI.create(OP_ONE))).thenReturn(true);

        propertiesHandler.addProperties(INDIVIDUAL, Collections.singletonMap(assertion, values));
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(managerMock).applyChanges(captor.capture());
        verifyAddedAxioms(values, captor);
    }

    private void verifyAddedAxioms(Set<Value<?>> values, ArgumentCaptor<List> captor) {
        for (Object ob : captor.getValue()) {
            assertTrue(ob instanceof AddAxiom);
            final AddAxiom ax = (AddAxiom) ob;
            assertTrue(ax.getAxiom() instanceof OWLObjectPropertyAssertionAxiom);
            final OWLObjectPropertyAssertionAxiom dp = (OWLObjectPropertyAssertionAxiom) ax.getAxiom();
            assertTrue(values.contains(new Value<>(NamedResource.create(
                    dp.getObject().asOWLNamedIndividual().getIRI().toURI()))));
        }
    }

    @Test
    public void addPropertiesAddsObjectPropertyAssertionsForUnknownAssertions() throws Exception {
        final Assertion assertion = Assertion.createObjectPropertyAssertion(OP_ONE, false);
        final Set<Value<?>> values = new HashSet<>(Arrays.asList(
                new Value<>(NamedResource.create("http://individualOne")),
                new Value<>(NamedResource.create("http://individualTwo"))));

        propertiesHandler.addProperties(INDIVIDUAL, Collections.singletonMap(assertion, values));
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(managerMock).applyChanges(captor.capture());
        verifyAddedAxioms(values, captor);
    }

    @Test
    public void removePropertiesRemovesSpecifiedPropertyValues() throws Exception {
        final String prop = "http://krizik.felk.cvut.cz/jopa#property";
        when(ontologyMock.containsDataPropertyInSignature(IRI.create(prop))).thenReturn(true);
        final Assertion assertion = Assertion.createPropertyAssertion(URI.create(prop), false);
        final Map<Assertion, Set<Value<?>>> toRemove = Collections
                .singletonMap(assertion, Collections.singleton(new Value<>("Test")));

        propertiesHandler.removeProperties(INDIVIDUAL, toRemove);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(managerMock).applyChanges(captor.capture());
        final List changes = captor.getValue();
        assertEquals(1, changes.size());
    }
}
