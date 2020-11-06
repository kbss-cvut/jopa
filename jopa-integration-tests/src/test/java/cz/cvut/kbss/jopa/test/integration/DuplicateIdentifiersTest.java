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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class DuplicateIdentifiersTest extends IntegrationTestBase {

    private OWLClassA entityA;

    @BeforeEach
    protected void setUp() throws Exception {
        super.setUp();
        initInstances();
    }

    private void initInstances() {
        this.entityA = new OWLClassA(Generators.generateUri());
        entityA.setStringAttribute("aStringAttribute");
    }

    @Test
    void persistObjectTwiceInPersistenceContextIsLegal() throws Exception {
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
    void persistTwoInstancesOfDifferentClassesWithSameIdentifierInOnePersistenceContextIsIllegal() {
        final OWLClassB entityB = new OWLClassB();
        entityB.setUri(entityA.getUri());

        final OWLEntityExistsException result = assertThrows(OWLEntityExistsException.class, () -> {
            em.getTransaction().begin();
            em.persist(entityA);
            em.persist(entityB);
            em.getTransaction().commit();
        });
        assertThat(result.getMessage(), containsString(entityA.getUri().toString()));
        assertThat(result.getMessage(), containsString("already managed in the persistence context."));
    }

    @Test
    void persistTwoInstancesOfDifferentClassesWithSameIdentifierInDifferentPersistenceContextsIsLegal()
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
    void mergeInstanceTwiceInTwoPersistenceContextsIsLegal() throws Exception {
        final NamedResource subject = NamedResource.create(entityA.getUri());
        final Assertion stringAss = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false);

        em.getTransaction().begin();
        em.persist(entityA);
        em.getTransaction().commit();
        initAxiomsForOWLClassA(subject, stringAss, entityA.getStringAttribute());
        when(connectionMock.contains(
                new AxiomImpl<>(subject, Assertion.createClassAssertion(false),
                        new Value<>(NamedResource.create(OWLClassA.getClassIri()))), Collections.emptySet())).thenReturn(true);

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

    @Test
    void mergeTwoInstancesWithTheSameIdentifierInTwoPersistenceContextsIsLegal() throws Exception {
        final NamedResource subject = NamedResource.create(entityA.getUri());
        final Assertion stringAssA = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false);
        final Assertion stringAssB = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_B_STRING_ATTRIBUTE), false);

        em.getTransaction().begin();
        em.persist(entityA);
        em.getTransaction().commit();
        initAxiomsForOWLClassA(subject, stringAssA, entityA.getStringAttribute());

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
        desc.addAssertion(Assertion.createUnspecifiedPropertyAssertion(false));
        when(connectionMock.find(desc)).thenReturn(axioms);
        when(connectionMock.contains(classAssertion, Collections.emptySet())).thenReturn(true);
    }

    @Test
    void mergeSameInstanceMultipleTimesInOnePersistenceContextIsLegal() throws Exception {
        final NamedResource subject = NamedResource.create(entityA.getUri());
        final Assertion stringAssA = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false);
        initAxiomsForOWLClassA(subject, stringAssA, entityA.getStringAttribute());

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
    void mergeTwoInstancesOfDifferentClassesWithTheSameIdentifierIntoOnePersistenceContextIsIllegal()
            throws Exception {
        final NamedResource subject = NamedResource.create(entityA.getUri());
        final Assertion stringAssA = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false);
        final Assertion stringAssB = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_B_STRING_ATTRIBUTE), false);
        initAxiomsForOWLClassA(subject, stringAssA, entityA.getStringAttribute());
        final OWLClassB entityB = new OWLClassB();
        entityB.setUri(entityA.getUri());
        entityB.setStringAttribute("bStringAttribute");
        initAxiomsForOWLClassB(entityB, subject, stringAssB);

        assertThrows(OWLEntityExistsException.class, () -> {
            entityA.setStringAttribute("updatedStringAttribute");
            em.getTransaction().begin();
            em.merge(entityA);
            em.merge(entityB);
            em.getTransaction().commit();
            verify(connectionMock).update(any(AxiomValueDescriptor.class));// Just for the entityA instance
        });
    }

    @Test
    void mergeTwoInstancesOfTheSameClassWithTheSameIdentifierIntoTheSamePersistenceContextIsLegal()
            throws Exception {
        final NamedResource subject = NamedResource.create(entityA.getUri());
        final Assertion stringAssA = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false);
        initAxiomsForOWLClassA(subject, stringAssA, entityA.getStringAttribute());

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
    void findTwiceInOnePersistenceContextWithTheSameIdentifierAndTypeReturnsTheSameInstance() throws Exception {
        final NamedResource subject = NamedResource.create(entityA.getUri());
        final Assertion stringAssA = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false);
        initAxiomsForOWLClassA(subject, stringAssA, entityA.getStringAttribute());

        final OWLClassA aOne = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(aOne);
        final OWLClassA aTwo = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(aTwo);
        assertSame(aOne, aTwo);
    }

    @Test
    void findIndividualAsDifferentTypeThanIsAlreadyLoadedInPersistenceContextIsIllegal() throws Exception {
        final NamedResource subject = NamedResource.create(entityA.getUri());
        final Assertion stringAssA = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false);
        final Assertion stringAssB = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_B_STRING_ATTRIBUTE), false);
        initAxiomsForOWLClassA(subject, stringAssA, entityA.getStringAttribute());
        final OWLClassB entityB = new OWLClassB();
        entityB.setUri(entityA.getUri());
        entityB.setStringAttribute("bStringAttribute");
        initAxiomsForOWLClassB(entityB, subject, stringAssB);

        final OWLClassA aOne = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(aOne);
        assertThrows(OWLEntityExistsException.class, () -> em.find(OWLClassB.class, entityB.getUri()));
    }
}
