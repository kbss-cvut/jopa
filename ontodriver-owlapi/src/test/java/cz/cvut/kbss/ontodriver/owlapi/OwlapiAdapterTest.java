/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import com.google.common.collect.Multimap;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.change.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.owlapi.change.SubjectDataPropertyRemove;
import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;
import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.environment.Generator;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAnnotationValue;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyExpression;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class OwlapiAdapterTest {

    private static final URI ID = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#EntityA");
    private static final NamedResource INDIVIDUAL = NamedResource.create(ID);

    @Mock
    private Connector connectorMock;

    private OntologySnapshot ontologySnapshot;

    private OWLOntology ontology;

    @Mock
    private OWLReasoner reasonerMock;

    private OWLDataFactory factory;

    private OwlapiAdapter sut;

    @BeforeEach
    void setUp() throws Exception {
        final OntologySnapshot snapshot = TestUtils.initRealOntology(reasonerMock);
        this.ontology = spy(snapshot.ontology());
        this.factory = snapshot.dataFactory();
        this.ontologySnapshot = new OntologySnapshot(ontology, snapshot.ontologyManager(), factory, reasonerMock);
        when(connectorMock.getOntologySnapshot()).thenReturn(ontologySnapshot);

        this.sut = spy(new OwlapiAdapter(connectorMock));
    }

    @Test
    void commitSendsChangesToConnector() throws Exception {
        startTransaction();
        sut.addTransactionalChanges(Collections.singletonList(mock(TransactionalChange.class)));
        sut.commit();
        verify(connectorMock).applyChanges(any());
    }

    private void startTransaction() throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        final Method startTransaction = OwlapiAdapter.class.getDeclaredMethod("startTransactionIfNotActive");
        startTransaction.setAccessible(true);
        startTransaction.invoke(sut);
    }

    @Test
    void rollbackCausesChangesToEmpty() throws Exception {
        startTransaction();
        sut.addTransactionalChanges(Collections.singletonList(mock(TransactionalChange.class)));
        sut.rollback();
        sut.commit();
        verify(connectorMock, never()).applyChanges(any());
    }

    @Test
    void testIsConsistentWithCorrectContext() {
        final URI uri = getOntologyUri();
        when(reasonerMock.isConsistent()).thenReturn(Boolean.TRUE);

        assertTrue(sut.isConsistent(uri));
        verify(reasonerMock).isConsistent();
    }

    private URI getOntologyUri() {
        final Optional<IRI> iri = ontology.getOntologyID().getOntologyIRI();
        return iri.map(IRI::toURI).orElse(null);
    }

    @Test
    void isConsistentIgnoresContextInfo() {
        when(reasonerMock.isConsistent()).thenReturn(Boolean.TRUE);
        final URI ctx = URI.create("http://krizik.felk.cvut.cz/differentContext");
        assertTrue(sut.isConsistent(ctx));
        verify(reasonerMock).isConsistent();
    }

    @Test
    void testGetContexts() {
        final IRI ontologyIri = ontology.getOntologyID().getOntologyIRI().get();
        when(connectorMock.getOntologyUri()).thenReturn(ontologyIri.toURI());
        final URI uri = getOntologyUri();

        final List<URI> res = sut.getContexts();
        assertEquals(1, res.size());
        assertEquals(uri, res.get(0));
    }

    @Test
    void testContainsUnknownContext() {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.CLASS, false);
        final URI context = URI.create("http://krizik.felk.cvut.cz/jopa/different");
        boolean res = sut.containsAxiom(axiom, Collections.singleton(context));
        assertFalse(res);
    }

    private Axiom<?> initAxiomForContains(Assertion.AssertionType assType, boolean inferred) {
        final NamedResource individual = NamedResource.create(
                "http://krizik.felk.cvut.cz/ontologies/jopa#IndividualOne");
        Assertion assertion;
        Value<?> value = switch (assType) {
            case CLASS -> {
                assertion = Assertion.createClassAssertion(inferred);
                yield new Value<>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#typeOne"));
            }
            case PROPERTY, OBJECT_PROPERTY -> {
                assertion = Assertion.createObjectPropertyAssertion(
                        URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#objectProperty"), inferred);
                yield new Value<>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#IndividualTwo"));
            }
            case DATA_PROPERTY -> {
                assertion = Assertion.createDataPropertyAssertion(
                        URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#dataProperty"), inferred);
                yield new Value<>("StringDataPropertyValue");
            }
            case ANNOTATION_PROPERTY -> {
                assertion = Assertion.createAnnotationPropertyAssertion(
                        URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#annotationProperty"), inferred);
                yield new Value<>("This is annotation value");
            }
        };
        return new AxiomImpl<>(individual, assertion, value);
    }

    private void addAxiomToOntology(Axiom<?> ax) {
        final OWLNamedIndividual individual =
                factory.getOWLNamedIndividual(IRI.create(ax.getSubject().getIdentifier()));
        OWLAxiom owlAxiom;
        switch (ax.getAssertion().getType()) {
            case CLASS:
                final OWLClass cls = factory.getOWLClass(IRI.create(ax.getValue().stringValue()));
                owlAxiom = factory.getOWLClassAssertionAxiom(cls, individual);
                break;
            case OBJECT_PROPERTY:
                final OWLNamedIndividual target =
                        factory.getOWLNamedIndividual(IRI.create(ax.getValue().stringValue()));
                final OWLObjectProperty op =
                        factory.getOWLObjectProperty(IRI.create(ax.getAssertion().getIdentifier()));
                owlAxiom = factory.getOWLObjectPropertyAssertionAxiom(op, individual, target);
                break;
            case DATA_PROPERTY:
                final OWLDataProperty dp = factory.getOWLDataProperty(IRI.create(ax.getAssertion().getIdentifier()));
                final OWLLiteral value = OwlapiUtils.createOWLLiteralFromValue(ax.getValue().getValue(),
                        OwlapiUtils.getAssertionLanguage(ax.getAssertion()));
                owlAxiom = factory.getOWLDataPropertyAssertionAxiom(dp, individual, value);
                break;
            case ANNOTATION_PROPERTY:
                final OWLAnnotationProperty ap =
                        factory.getOWLAnnotationProperty(IRI.create(ax.getAssertion().getIdentifier()));
                final OWLAnnotationValue val =
                        OwlapiUtils.createOWLLiteralFromValue(ax.getValue().getValue(),
                                OwlapiUtils.getAssertionLanguage(ax.getAssertion()));
                owlAxiom = factory.getOWLAnnotationAssertionAxiom(ap, individual.getIRI(), val);
                break;
            default:
                throw new IllegalArgumentException(ax.getAssertion().toString());
        }
        ontologySnapshot.ontologyManager().addAxiom(ontology, owlAxiom);
    }

    @Test
    void testContainsClassAssertionAxiomNoContext() {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.CLASS, false);
        addAxiomToOntology(axiom);
        boolean res = sut.containsAxiom(axiom, null);

        assertTrue(res);
        final ArgumentCaptor<OWLAxiom> captor = ArgumentCaptor.forClass(OWLAxiom.class);
        verify(ontology).containsAxiom(captor.capture());
        final OWLAxiom ax = captor.getValue();
        assertTrue(ax.isOfType(AxiomType.CLASS_ASSERTION));
        final Set<OWLClass> types = ax.classesInSignature().collect(Collectors.toSet());
        assertEquals(1, types.size());
        final OWLClass cls = types.iterator().next();
        assertEquals(axiom.getValue().getValue(), cls.getIRI().toURI());
    }

    @Test
    void testContainsObjectPropertyAssertionInferred() {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.OBJECT_PROPERTY, true);
        final URI ctx = getOntologyUri();
        when(reasonerMock.isEntailed(any(OWLAxiom.class))).thenReturn(Boolean.TRUE);
        boolean res = sut.containsAxiom(axiom, Collections.singleton(ctx));

        assertTrue(res);
        final ArgumentCaptor<OWLAxiom> captor = ArgumentCaptor.forClass(OWLAxiom.class);
        verify(reasonerMock).isEntailed(captor.capture());
        final OWLAxiom ax = captor.getValue();
        assertTrue(ax.isOfType(AxiomType.OBJECT_PROPERTY_ASSERTION));
        final Set<OWLNamedIndividual> individuals = ax.individualsInSignature().collect(Collectors.toSet());
        assertEquals(2, individuals.size());
        assertTrue(individuals.contains(factory.getOWLNamedIndividual(IRI.create(axiom.getSubject().toString()))));
        assertTrue(individuals.contains(factory.getOWLNamedIndividual(IRI.create(axiom.getValue().stringValue()))));
        final Set<OWLObjectProperty> objProperties = ax.objectPropertiesInSignature().collect(Collectors.toSet());
        assertEquals(1, objProperties.size());
        assertEquals(axiom.getAssertion().getIdentifier(), objProperties.iterator().next().getIRI().toURI());
    }

    @Test
    void testContainsDataProperty() {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.DATA_PROPERTY, false);
        addAxiomToOntology(axiom);
        boolean res = sut.containsAxiom(axiom, null);

        assertTrue(res);
        final ArgumentCaptor<OWLAxiom> captor = ArgumentCaptor.forClass(OWLAxiom.class);
        verify(ontology).containsAxiom(captor.capture());
        final OWLAxiom ax = captor.getValue();
        assertTrue(ax.isOfType(AxiomType.DATA_PROPERTY_ASSERTION));
        final Set<OWLDataProperty> dataProperties = ax.dataPropertiesInSignature().collect(Collectors.toSet());
        assertEquals(1, dataProperties.size());
        assertEquals(axiom.getAssertion().getIdentifier(), dataProperties.iterator().next().getIRI().toURI());
        final Set<OWLNamedIndividual> individuals = ax.individualsInSignature().collect(Collectors.toSet());
        assertEquals(1, individuals.size());
        assertEquals(axiom.getSubject().getIdentifier(), individuals.iterator().next().getIRI().toURI());
        assertInstanceOf(OWLDataPropertyAssertionAxiom.class, ax);
        assertEquals(axiom.getValue().stringValue(), ((OWLDataPropertyAssertionAxiom) ax).getObject().getLiteral());
    }

    @Test
    void testContainsAnnotationPropertyInferred() {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.ANNOTATION_PROPERTY, true);
        when(reasonerMock.isEntailed(any(OWLAxiom.class))).thenReturn(Boolean.TRUE);
        boolean res = sut.containsAxiom(axiom, null);

        assertTrue(res);
        final ArgumentCaptor<OWLAxiom> captor = ArgumentCaptor.forClass(OWLAxiom.class);
        verify(reasonerMock).isEntailed(captor.capture());
        final OWLAxiom ax = captor.getValue();
        assertTrue(ax.isOfType(AxiomType.ANNOTATION_ASSERTION));
        final Set<OWLAnnotationProperty> annotationProperties =
                ax.annotationPropertiesInSignature().collect(Collectors.toSet());
        assertEquals(1, annotationProperties.size());
        assertEquals(axiom.getAssertion().getIdentifier(), annotationProperties.iterator().next().getIRI().toURI());
        assertInstanceOf(OWLAnnotationAssertionAxiom.class, ax);
        final OWLAnnotationAssertionAxiom assertionAxiom = (OWLAnnotationAssertionAxiom) ax;
        assertEquals(axiom.getValue().stringValue(),
                assertionAxiom.getAnnotation().getValue().asLiteral().get().getLiteral());
    }

    @Test
    void persistIndividualInMultipleClassesIsLegal() {
        final AxiomValueDescriptor descriptorOne = new AxiomValueDescriptor(INDIVIDUAL);
        final String typeA = "http://krizik.felk.cvut.cz/typeA";
        descriptorOne.addAssertionValue(Assertion.createClassAssertion(false), new Value<>(URI.create(typeA)));
        final String propA = "http://krizik.felk.cvut.cz/propertyA";
        descriptorOne
                .addAssertionValue(Assertion.createDataPropertyAssertion(URI.create(propA), false), new Value<>(false));
        sut.persist(descriptorOne);
        sut.commit();
        final AxiomValueDescriptor descriptorTwo = new AxiomValueDescriptor(INDIVIDUAL);
        final String typeB = "http://krizik.felk.cvut.cz/typeB";
        descriptorTwo.addAssertionValue(Assertion.createClassAssertion(false), new Value<>(URI.create(typeB)));
        final String propB = "http://krizik.felk.cvut.cz/propertyB";
        final double bValue = 3.1419;
        descriptorTwo.addAssertionValue(Assertion.createDataPropertyAssertion(URI.create(propB), false),
                new Value<>(bValue));
        sut.persist(descriptorTwo);

        final OWLNamedIndividual individual = factory.getOWLNamedIndividual(IRI.create(INDIVIDUAL.getIdentifier()));
        final Collection<OWLClassExpression> classes =
                EntitySearcher.getTypes(individual, ontology).collect(Collectors.toSet());
        assertTrue(classes.contains(factory.getOWLClass(IRI.create(typeA))));
        assertTrue(classes.contains(factory.getOWLClass(IRI.create(typeB))));
        final Multimap<OWLDataPropertyExpression, OWLLiteral> properties =
                EntitySearcher.getDataPropertyValues(individual, ontology);
        assertTrue(
                properties.containsEntry(factory.getOWLDataProperty(IRI.create(propA)), factory.getOWLLiteral(false)));
        assertTrue(
                properties.containsEntry(factory.getOWLDataProperty(IRI.create(propB)), factory.getOWLLiteral(bValue)));
    }

    @Test
    void startingTransactionInitializesOntologySnapshotAndExecutorFactory() throws Exception {
        final Field executorFactoryField = OwlapiAdapter.class.getDeclaredField("statementExecutorFactory");
        executorFactoryField.setAccessible(true);
        assertNull(executorFactoryField.get(sut));
        startTransaction();
        verify(connectorMock).getOntologySnapshot();
        assertNotNull(executorFactoryField.get(sut));
    }

    @Test
    void transactionCommitClosesTransactionalSnapshot() throws Exception {
        startTransaction();
        sut.addTransactionalChanges(Collections.singletonList(mock(TransactionalChange.class)));
        sut.commit();
        verify(connectorMock).closeSnapshot(ontologySnapshot);
    }

    @Test
    void transactionRollbackClosesTransactionalSnapshot() throws Exception {
        startTransaction();
        sut.addTransactionalChanges(Collections.singletonList(mock(TransactionalChange.class)));
        sut.rollback();
        verify(connectorMock).closeSnapshot(ontologySnapshot);
    }

    @Test
    void unwrapReturnsTheAdapterWhenClassMatches() throws Exception {
        assertSame(sut, sut.unwrap(OwlapiAdapter.class));
    }

    @Test
    void unwrapReturnsOntologySnapshotWhenOwlOntologyClassIsPassedIn() throws Exception {
        assertSame(ontology, sut.unwrap(OWLOntology.class));
    }

    @Test
    void throwsDriverExceptionWhenUnsupportedClassIsPassedToUnwrap() {
        assertThrows(OwlapiDriverException.class, () -> sut.unwrap(Connection.class));
    }

    @Test
    void unwrapReturnsOwlReasonerWhenClassMatches() throws Exception {
        assertSame(reasonerMock, sut.unwrap(OWLReasoner.class));
    }

    @Test
    void isInferredChecksWhetherAxiomIsEntailedByReasonerButNotContainedInOntology() {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.CLASS, true);
        when(reasonerMock.isEntailed(any(OWLAxiom.class))).thenReturn(true);

        assertTrue(sut.isInferred(axiom, Collections.emptySet()));
        final OWLAxiom owlAxiom = factory.getOWLClassAssertionAxiom(factory.getOWLClass(axiom.getValue()
                                                                                             .stringValue()), factory.getOWLNamedIndividual(axiom.getSubject()
                                                                                                                                                 .getIdentifier()
                                                                                                                                                 .toString()));
        verify(reasonerMock).isEntailed(owlAxiom);
        verify(ontology).containsAxiom(owlAxiom);
    }

    @Test
    void isInferredReturnsFalseWhenAxiomIsEntailedAndExistsInOntology() {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.CLASS, true);
        when(reasonerMock.isEntailed(any(OWLAxiom.class))).thenReturn(true);
        addAxiomToOntology(axiom);

        assertFalse(sut.isInferred(axiom, Collections.emptySet()));
        final OWLAxiom owlAxiom = factory.getOWLClassAssertionAxiom(factory.getOWLClass(axiom.getValue()
                                                                                             .stringValue()), factory.getOWLNamedIndividual(axiom.getSubject()
                                                                                                                                                 .getIdentifier()
                                                                                                                                                 .toString()));
        verify(reasonerMock).isEntailed(owlAxiom);
        verify(ontology).containsAxiom(owlAxiom);
    }

    @Test
    void isInferredFlushesBufferedChangesOnReasonerBeforeCheckingAxiomEntailment() {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.CLASS, true);
        when(reasonerMock.isEntailed(any(OWLAxiom.class))).thenReturn(true);

        assertTrue(sut.isInferred(axiom, Collections.emptySet()));
        final InOrder inOrder = inOrder(reasonerMock);
        inOrder.verify(reasonerMock).flush();
        inOrder.verify(reasonerMock).isEntailed(any(OWLAxiom.class));
    }

    @Test
    void addTransactionalChangesRemovesAddAxiomsOverriddenByRemoveSubjectPropertyChanges() throws Exception {
        final OWLNamedIndividual subject = factory.getOWLNamedIndividual(IRI.create(INDIVIDUAL.getIdentifier()));
        final OWLDataProperty property = factory.getOWLDataProperty(IRI.create(Generator.generateUri()));
        final MutableAddAxiom add = new MutableAddAxiom(ontology, factory.getOWLDataPropertyAssertionAxiom(property, subject, 117));
        startTransaction();
        sut.addTransactionalChanges(List.of(add));
        sut.addTransactionalChanges(List.of(new SubjectDataPropertyRemove(subject, property)));
        sut.commit();
        verify(connectorMock).applyChanges(List.of(new SubjectDataPropertyRemove(subject, property)));
    }
}
