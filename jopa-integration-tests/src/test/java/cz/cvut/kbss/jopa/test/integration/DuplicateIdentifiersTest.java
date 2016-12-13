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
package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.ArgumentCaptor;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;

public class DuplicateIdentifiersTest extends IntegrationTestBase {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    private OWLClassA entityA;

    @Before
    public void setUp() throws Exception {
        super.setUp();

        initInstances();
    }

    private void initInstances() {
        this.entityA = new OWLClassA(Generators.generateUri());
        entityA.setStringAttribute("aStringAttribute");
    }

    @Test
    public void persistObjectTwiceInPersistenceContextIsLegal() throws Exception {
        em.getTransaction().begin();
        em.persist(entityA);
        entityA.setStringAttribute("UpdatedString");
        em.persist(entityA);
        em.getTransaction().commit();

        verify(connectionMock).persist(any(AxiomValueDescriptor.class));
        final ArgumentCaptor<AxiomValueDescriptor> captor = ArgumentCaptor.forClass(AxiomValueDescriptor.class);
        verify(connectionMock).update(captor.capture());
        final AxiomValueDescriptor descriptor = captor.getValue();
        assertEquals(1, descriptor.getAssertions().size());
        final List<Value<?>> values = descriptor.getAssertionValues(Assertion.createDataPropertyAssertion(URI.create(
                Vocabulary.P_A_STRING_ATTRIBUTE), false));
        assertEquals(1, values.size());
        assertEquals(entityA.getStringAttribute(), values.get(0).getValue());
    }

    @Test
    public void persistTwoInstancesOfDifferentClassesWithSameIdentifierInOnePersistenceContextIsIllegal()
            throws Exception {
        thrown.expect(OWLEntityExistsException.class);
        thrown.expectMessage(
                "An entity with URI " + entityA.getUri() + " is already present in the current persistence context.");
        final OWLClassB entityB = new OWLClassB();
        entityB.setUri(entityA.getUri());

        em.getTransaction().begin();
        em.persist(entityA);
        em.persist(entityB);
        em.getTransaction().commit();
    }

    @Test
    public void persistTwoInstancesOfDifferentClassesWithSameIdentifierInDifferentPersistenceContextsIsLegal()
            throws Exception {
        final OWLClassB entityB = new OWLClassB();
        entityB.setUri(entityA.getUri());
        entityB.setStringAttribute("bStringAttribute");

        em.getTransaction().begin();
        em.persist(entityA);
        em.getTransaction().commit();
        when(connectionMock.contains(
                new AxiomImpl<>(NamedResource.create(entityA.getUri()), Assertion.createClassAssertion(false),
                        new Value<>(NamedResource.create(OWLClassA.getClassIri()))), null)).thenReturn(true);

        em.getTransaction().begin();
        em.persist(entityB);
        em.getTransaction().commit();
        verify(connectionMock, times(2)).persist(any(AxiomValueDescriptor.class));
    }

    @Test
    public void mergeInstanceTwiceInTwoPersistenceContextsIsLegal() throws Exception {
        final NamedResource subject = NamedResource.create(entityA.getUri());
        final Assertion stringAss = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false);

        em.getTransaction().begin();
        em.persist(entityA);
        em.getTransaction().commit();
        initAxiomsForOWLClassA(subject, stringAss);
        when(connectionMock.contains(
                new AxiomImpl<>(subject, Assertion.createClassAssertion(false),
                        new Value<>(NamedResource.create(OWLClassA.getClassIri()))), null)).thenReturn(true);

        final String newStringOne = "newStringAttributeOne";
        entityA.setStringAttribute(newStringOne);
        em.getTransaction().begin();
        em.merge(entityA);
        em.getTransaction().commit();
        final ArgumentCaptor<AxiomValueDescriptor> captor = ArgumentCaptor.forClass(AxiomValueDescriptor.class);

        final String newStringTwo = "newStringAttributeTwo";
        entityA.setStringAttribute(newStringTwo);
        em.getTransaction().begin();
        em.merge(entityA);
        em.getTransaction().commit();

        verify(connectionMock, times(2)).update(captor.capture());
        final AxiomValueDescriptor firstUpdate = captor.getAllValues().get(0);
        assertEquals(1, firstUpdate.getAssertionValues(stringAss).size());
        assertEquals(newStringOne, firstUpdate.getAssertionValues(stringAss).get(0).getValue());
        final AxiomValueDescriptor secondUpdate = captor.getAllValues().get(1);
        assertEquals(1, secondUpdate.getAssertionValues(stringAss).size());
        assertEquals(newStringTwo, secondUpdate.getAssertionValues(stringAss).get(0).getValue());
    }

    private void initAxiomsForOWLClassA(NamedResource subject, Assertion stringAss) throws OntoDriverException {
        final List<Axiom<?>> axioms = new ArrayList<>();
        final Axiom<?> classAssertion = new AxiomImpl<>(subject, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(OWLClassA.getClassIri())));
        axioms.add(classAssertion);
        axioms.add(new AxiomImpl<>(subject, stringAss, new Value<>(entityA.getStringAttribute())));
        final AxiomDescriptor desc = new AxiomDescriptor(subject);
        desc.addAssertion(Assertion.createClassAssertion(false));
        desc.addAssertion(stringAss);
        when(connectionMock.find(desc)).thenReturn(axioms);
        when(connectionMock.contains(classAssertion, null)).thenReturn(true);
    }

    @Test
    public void mergeTwoInstancesWithTheSameIdentifierInTwoPersistenceContextsIsLegal() throws Exception {
        final NamedResource subject = NamedResource.create(entityA.getUri());
        final Assertion stringAssA = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false);
        final Assertion stringAssB = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_B_STRING_ATTRIBUTE), false);

        em.getTransaction().begin();
        em.persist(entityA);
        em.getTransaction().commit();
        initAxiomsForOWLClassA(subject, stringAssA);

        final OWLClassB entityB = new OWLClassB();
        entityB.setUri(entityA.getUri());
        entityB.setStringAttribute("bStringAttribute");
        em.getTransaction().begin();
        em.persist(entityB);
        em.getTransaction().commit();
        initAxiomsForOWLClassB(entityB, subject, stringAssB);

        final String newStringA = "newStringAttributeA";
        entityA.setStringAttribute(newStringA);
        em.getTransaction().begin();
        em.merge(entityA);
        em.getTransaction().commit();

        final String newStringB = "newStringAttributeB";
        entityB.setStringAttribute(newStringB);
        em.getTransaction().begin();
        em.merge(entityB);
        em.getTransaction().commit();

        final ArgumentCaptor<AxiomValueDescriptor> captor = ArgumentCaptor.forClass(AxiomValueDescriptor.class);
        verify(connectionMock, times(2)).update(captor.capture());
        final AxiomValueDescriptor firstUpdate = captor.getAllValues().get(0);
        assertEquals(1, firstUpdate.getAssertionValues(stringAssA).size());
        assertEquals(newStringA, firstUpdate.getAssertionValues(stringAssA).get(0).getValue());
        final AxiomValueDescriptor secondUpdate = captor.getAllValues().get(1);
        assertEquals(1, secondUpdate.getAssertionValues(stringAssB).size());
        assertEquals(newStringB, secondUpdate.getAssertionValues(stringAssB).get(0).getValue());
    }

    private void initAxiomsForOWLClassB(OWLClassB entityB, NamedResource subject, Assertion stringAss)
            throws OntoDriverException {
        final List<Axiom<?>> axioms = new ArrayList<>();
        final Axiom<?> classAssertion = new AxiomImpl<>(subject, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_B)));
        axioms.add(classAssertion);
        axioms.add(new AxiomImpl<>(subject, stringAss, new Value<>(entityB.getStringAttribute())));
        final AxiomDescriptor desc = new AxiomDescriptor(subject);
        desc.addAssertion(Assertion.createClassAssertion(false));
        desc.addAssertion(stringAss);
        when(connectionMock.find(desc)).thenReturn(axioms);
        when(connectionMock.contains(classAssertion, null)).thenReturn(true);
    }

    @Test
    public void mergeSameInstanceMultipleTimesInOnePersistenceContextIsLegal() throws Exception {
        final NamedResource subject = NamedResource.create(entityA.getUri());
        final Assertion stringAssA = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false);
        initAxiomsForOWLClassA(subject, stringAssA);

        final String updateOne = "updatedString";
        entityA.setStringAttribute(updateOne);
        em.getTransaction().begin();
        em.merge(entityA);
        final String updateTwo = "updatedStringAgain";
        entityA.setStringAttribute(updateTwo);
        em.merge(entityA);
        em.getTransaction().commit();

        final ArgumentCaptor<AxiomValueDescriptor> captor = ArgumentCaptor.forClass(AxiomValueDescriptor.class);
        verify(connectionMock, times(2)).update(captor.capture());
        final List<Value<?>> valuesOne = captor.getAllValues().get(0).getAssertionValues(stringAssA);
        assertEquals(1, valuesOne.size());
        assertEquals(updateOne, valuesOne.get(0).getValue());

        final List<Value<?>> valuesTwo = captor.getAllValues().get(1).getAssertionValues(stringAssA);
        assertEquals(1, valuesTwo.size());
        assertEquals(updateTwo, valuesTwo.get(0).getValue());
    }

    @Test
    public void mergeTwoInstancesOfDifferentClassesWithTheSameIdentifierIntoOnePersistenceContextIsIllegal()
            throws Exception {
        thrown.expect(OWLEntityExistsException.class);
        thrown.expectMessage(
                "An entity with URI " + entityA.getUri() + " is already present in the current persistence context.");
        final NamedResource subject = NamedResource.create(entityA.getUri());
        final Assertion stringAssA = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false);
        final Assertion stringAssB = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_B_STRING_ATTRIBUTE), false);
        initAxiomsForOWLClassA(subject, stringAssA);
        final OWLClassB entityB = new OWLClassB();
        entityB.setUri(entityA.getUri());
        entityB.setStringAttribute("bStringAttribute");
        initAxiomsForOWLClassB(entityB, subject, stringAssB);

        entityA.setStringAttribute("updatedStringAttribute");
        em.getTransaction().begin();
        em.merge(entityA);
        em.merge(entityB);
        em.getTransaction().commit();
        verify(connectionMock).update(any(AxiomValueDescriptor.class));// Just for the entityA instance
    }

    @Test
    public void mergeTwoInstancesOfTheSameClassWithTheSameIdentifierIntoTheSamePersistenceContextIsLegal()
            throws Exception {
        final NamedResource subject = NamedResource.create(entityA.getUri());
        final Assertion stringAssA = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false);
        initAxiomsForOWLClassA(subject, stringAssA);

        final String updateOne = "update";
        entityA.setStringAttribute(updateOne);
        em.getTransaction().begin();
        em.merge(entityA);
        final OWLClassA secondA = new OWLClassA(entityA.getUri());
        final String updateTwo = "updatedAgain";
        secondA.setStringAttribute(updateTwo);
        em.merge(secondA);
        em.getTransaction().commit();

        final ArgumentCaptor<AxiomValueDescriptor> captor = ArgumentCaptor.forClass(AxiomValueDescriptor.class);
        verify(connectionMock, times(2)).update(captor.capture());
        final List<Value<?>> valuesOne = captor.getAllValues().get(0).getAssertionValues(stringAssA);
        assertEquals(1, valuesOne.size());
        assertEquals(updateOne, valuesOne.get(0).getValue());

        final List<Value<?>> valuesTwo = captor.getAllValues().get(1).getAssertionValues(stringAssA);
        assertEquals(1, valuesTwo.size());
        assertEquals(updateTwo, valuesTwo.get(0).getValue());
    }

    @Test
    public void findTwiceInOnePersistenceContextWithTheSameIdentifierAndTypeReturnsTheSameInstance() throws Exception {
        final NamedResource subject = NamedResource.create(entityA.getUri());
        final Assertion stringAssA = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false);
        initAxiomsForOWLClassA(subject, stringAssA);

        final OWLClassA aOne = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(aOne);
        final OWLClassA aTwo = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(aTwo);
        assertSame(aOne, aTwo);
    }

    @Test
    public void findIndividualAsDifferentTypeThanIsAlreadyLoadedInPersistenceContextIsIllegal() throws Exception {
        thrown.expect(OWLEntityExistsException.class);
        thrown.expectMessage(
                "An entity with URI " + entityA.getUri() + " is already present in the current persistence context.");
        final NamedResource subject = NamedResource.create(entityA.getUri());
        final Assertion stringAssA = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false);
        final Assertion stringAssB = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_B_STRING_ATTRIBUTE), false);
        initAxiomsForOWLClassA(subject, stringAssA);
        final OWLClassB entityB = new OWLClassB();
        entityB.setUri(entityA.getUri());
        entityB.setStringAttribute("bStringAttribute");
        initAxiomsForOWLClassB(entityB, subject, stringAssB);

        final OWLClassA aOne = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(aOne);
        em.find(OWLClassB.class, entityB.getUri());
    }
}
