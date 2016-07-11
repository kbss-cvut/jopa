/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import com.google.common.base.Optional;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Set;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

// TODO Try to rewrite this to a real ontology
public class OwlapiAdapterTest {

    private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#EntityA");
    private static final NamedResource INDIVIDUAL = NamedResource.create(PK);

    @Mock
    private Connector connectorMock;
    @Mock
    private OntologySnapshot snapshotMock;
    @Mock
    private OWLOntology ontologyMock;
    @Mock
    private OWLReasoner reasonerMock;

    private OWLDataFactory factory;

    private OwlapiAdapter adapter;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(connectorMock.getOntologySnapshot()).thenReturn(snapshotMock);
        when(snapshotMock.getOntology()).thenReturn(ontologyMock);
        this.factory = new OWLDataFactoryImpl();
        when(snapshotMock.getDataFactory()).thenReturn(factory);
        when(snapshotMock.getReasoner()).thenReturn(reasonerMock);

        this.adapter = spy(new OwlapiAdapter(connectorMock, new Configuration(
                OntologyStorageProperties.driver(OwlapiDataSource.class.getName()).physicalUri("testFile").build())));
    }

    @Test
    public void commitSendsChangesToConnector() throws Exception {
        startTransaction();
        adapter.addTransactionalChanges(Collections.singletonList(mock(OWLOntologyChange.class)));
        adapter.commit();
        verify(connectorMock).applyChanges(any());
    }

    private void startTransaction() throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        final Method startTransaction = OwlapiAdapter.class.getDeclaredMethod("startTransactionIfNotActive");
        startTransaction.setAccessible(true);
        startTransaction.invoke(adapter);
    }

    @Test
    public void rollbackCausesChangesToEmpty() throws Exception {
        startTransaction();
        adapter.addTransactionalChanges(Collections.singletonList(mock(OWLOntologyChange.class)));
        adapter.rollback();
        adapter.commit();
        verify(connectorMock, never()).applyChanges(any());
    }

    @Test
    public void testIsConsistentWithCorrectContext() throws Exception {
        final IRI iri = setupOntologyIri();
        final OWLReasoner reasonerMock = mock(OWLReasoner.class);
        when(snapshotMock.getReasoner()).thenReturn(reasonerMock);
        when(reasonerMock.isConsistent()).thenReturn(Boolean.TRUE);

        assertTrue(adapter.isConsistent(iri.toURI()));
        verify(reasonerMock).isConsistent();
    }

    private IRI setupOntologyIri() {
        final IRI iri = IRI.create("http://krizik.felk.cvut.cz/ontologies/jopa");
        final OWLOntologyID id = new OWLOntologyID(Optional.fromNullable(iri), Optional.absent());
        when(ontologyMock.getOntologyID()).thenReturn(id);
        return iri;
    }

    @Test
    public void isConsistentIgnoresContextInfo() throws Exception {
        setupOntologyIri();
        final OWLReasoner reasonerMock = mock(OWLReasoner.class);
        when(snapshotMock.getReasoner()).thenReturn(reasonerMock);
        when(reasonerMock.isConsistent()).thenReturn(Boolean.TRUE);
        final URI ctx = URI.create("http://krizik.felk.cvut.cz/differentContext");
        assertTrue(adapter.isConsistent(ctx));
        verify(reasonerMock).isConsistent();
    }

    @Test
    public void testGetContexts() throws Exception {
        final IRI iri = setupOntologyIri();

        final List<URI> res = adapter.getContexts();
        assertEquals(1, res.size());
        assertEquals(iri.toURI(), res.get(0));
    }

    @Test
    public void testContainsUnknownContext() throws Exception {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.CLASS, false);
        final URI context = URI.create("http://krizik.felk.cvut.cz/jopa/different");
        setupOntologyIri();
        boolean res = adapter.containsAxiom(axiom, context);
        assertFalse(res);
    }

    private Axiom<?> initAxiomForContains(Assertion.AssertionType assType, boolean inferred) {
        final NamedResource individual = NamedResource.create(
                "http://krizik.felk.cvut.cz/ontologies/jopa#IndividualOne");
        Assertion assertion = null;
        Value<?> value = null;
        switch (assType) {
            case CLASS:
                assertion = Assertion.createClassAssertion(inferred);
                value = new Value<>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#typeOne"));
                break;
            case PROPERTY:
            case OBJECT_PROPERTY:
                assertion = Assertion.createObjectPropertyAssertion(
                        URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#objectProperty"), inferred);
                value = new Value<>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#IndividualTwo"));
                break;
            case DATA_PROPERTY:
                assertion = Assertion.createDataPropertyAssertion(
                        URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#dataProperty"), inferred);
                value = new Value<>("StringDataPropertyValue");
                break;
            case ANNOTATION_PROPERTY:
                assertion = Assertion.createAnnotationPropertyAssertion(
                        URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#annotationProperty"), inferred);
                value = new Value<>("This is annotation value");
                break;
        }
        return new AxiomImpl<>(individual, assertion, value);
    }

    @Test
    public void testContainsClassAssertionAxiomNoContext() throws Exception {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.CLASS, false);
        when(ontologyMock.containsAxiom(any())).thenReturn(Boolean.TRUE);
        boolean res = adapter.containsAxiom(axiom, null);

        assertTrue(res);
        final ArgumentCaptor<OWLAxiom> captor = ArgumentCaptor.forClass(OWLAxiom.class);
        verify(ontologyMock).containsAxiom(captor.capture());
        final OWLAxiom ax = captor.getValue();
        assertTrue(ax.isOfType(AxiomType.CLASS_ASSERTION));
        final Set<OWLClass> types = ax.getClassesInSignature();
        assertEquals(1, types.size());
        final OWLClass cls = types.iterator().next();
        assertEquals(axiom.getValue().getValue(), cls.getIRI().toURI());
    }

    @Test
    public void testContainsObjectPropertyAssertionInferred() throws Exception {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.OBJECT_PROPERTY, true);
        final IRI ctx = setupOntologyIri();
        when(ontologyMock.containsAxiom(any())).thenReturn(Boolean.FALSE);
        final OWLReasoner reasonerMock = mock(OWLReasoner.class);
        when(snapshotMock.getReasoner()).thenReturn(reasonerMock);
        when(reasonerMock.isEntailed(any(OWLAxiom.class))).thenReturn(Boolean.TRUE);
        boolean res = adapter.containsAxiom(axiom, ctx.toURI());

        assertTrue(res);
        final ArgumentCaptor<OWLAxiom> captor = ArgumentCaptor.forClass(OWLAxiom.class);
        verify(reasonerMock).isEntailed(captor.capture());
        final OWLAxiom ax = captor.getValue();
        assertTrue(ax.isOfType(AxiomType.OBJECT_PROPERTY_ASSERTION));
        final Set<OWLNamedIndividual> individuals = ax.getIndividualsInSignature();
        assertEquals(2, individuals.size());
        assertTrue(individuals.contains(factory.getOWLNamedIndividual(IRI.create(axiom.getSubject().toString()))));
        assertTrue(individuals.contains(factory.getOWLNamedIndividual(IRI.create(axiom.getValue().stringValue()))));
        final Set<OWLObjectProperty> objProperties = ax.getObjectPropertiesInSignature();
        assertEquals(1, objProperties.size());
        assertEquals(axiom.getAssertion().getIdentifier(), objProperties.iterator().next().getIRI().toURI());
    }

    @Test
    public void testContainsDataProperty() throws Exception {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.DATA_PROPERTY, false);
        when(ontologyMock.containsAxiom(any())).thenReturn(Boolean.TRUE);
        boolean res = adapter.containsAxiom(axiom, null);

        assertTrue(res);
        final ArgumentCaptor<OWLAxiom> captor = ArgumentCaptor.forClass(OWLAxiom.class);
        verify(ontologyMock).containsAxiom(captor.capture());
        final OWLAxiom ax = captor.getValue();
        assertTrue(ax.isOfType(AxiomType.DATA_PROPERTY_ASSERTION));
        final Set<OWLDataProperty> dataProperties = ax.getDataPropertiesInSignature();
        assertEquals(1, dataProperties.size());
        assertEquals(axiom.getAssertion().getIdentifier(), dataProperties.iterator().next().getIRI().toURI());
        final Set<OWLNamedIndividual> individuals = ax.getIndividualsInSignature();
        assertEquals(1, individuals.size());
        assertEquals(axiom.getSubject().getIdentifier(), individuals.iterator().next().getIRI().toURI());
        assertTrue(ax instanceof OWLDataPropertyAssertionAxiom);
        assertEquals(axiom.getValue().stringValue(), ((OWLDataPropertyAssertionAxiom) ax).getObject().getLiteral());
    }

    @Test
    public void testContainsAnnotationPropertyInferred() throws Exception {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.ANNOTATION_PROPERTY, true);
        setupOntologyIri();
        final OWLReasoner reasonerMock = mock(OWLReasoner.class);
        when(snapshotMock.getReasoner()).thenReturn(reasonerMock);
        when(reasonerMock.isEntailed(any(OWLAxiom.class))).thenReturn(Boolean.TRUE);
        boolean res = adapter.containsAxiom(axiom, null);

        assertTrue(res);
        final ArgumentCaptor<OWLAxiom> captor = ArgumentCaptor.forClass(OWLAxiom.class);
        verify(reasonerMock).isEntailed(captor.capture());
        final OWLAxiom ax = captor.getValue();
        assertTrue(ax.isOfType(AxiomType.ANNOTATION_ASSERTION));
        final Set<OWLAnnotationProperty> annotationProperties = ax.getAnnotationPropertiesInSignature();
        assertEquals(1, annotationProperties.size());
        assertEquals(axiom.getAssertion().getIdentifier(), annotationProperties.iterator().next().getIRI().toURI());
        assertTrue(ax instanceof OWLAnnotationAssertionAxiom);
        final OWLAnnotationAssertionAxiom assertionAxiom = (OWLAnnotationAssertionAxiom) ax;
        assertEquals(axiom.getValue().stringValue(),
                assertionAxiom.getAnnotation().getValue().asLiteral().get().getLiteral());
    }


    @Test
    public void persistIndividualInMultipleClassesIsLegal() throws Exception {
        final AxiomValueDescriptor descriptorOne = new AxiomValueDescriptor(INDIVIDUAL);
        final String typeA = "http://krizik.felk.cvut.cz/typeA";
        descriptorOne.addAssertionValue(Assertion.createClassAssertion(false), new Value<>(URI.create(typeA)));
        final String propA = "http://krizik.felk.cvut.cz/propertyA";
        descriptorOne
                .addAssertionValue(Assertion.createDataPropertyAssertion(URI.create(propA), false), new Value<>(false));
        adapter.persist(descriptorOne);
        adapter.commit();
        final AxiomValueDescriptor descriptorTwo = new AxiomValueDescriptor(INDIVIDUAL);
        final String typeB = "http://krizik.felk.cvut.cz/typeB";
        descriptorTwo.addAssertionValue(Assertion.createClassAssertion(false), new Value<>(URI.create(typeB)));
        final String propB = "http://krizik.felk.cvut.cz/propertyB";
        descriptorTwo.addAssertionValue(Assertion.createDataPropertyAssertion(URI.create(propB), false),
                new Value<>(new Date()));
        adapter.persist(descriptorTwo);

        // TODO Verify that axioms were added
    }

    @Test
    public void startingTransactionInitializesOntologySnapshotAndExecutorFactory() throws Exception {
        final Field executorFactoryField = OwlapiAdapter.class.getDeclaredField("statementExecutorFactory");
        executorFactoryField.setAccessible(true);
        assertNull(executorFactoryField.get(adapter));
        startTransaction();
        verify(connectorMock).getOntologySnapshot();
        assertNotNull(executorFactoryField.get(adapter));
    }
}
