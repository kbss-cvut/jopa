/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.integration.virtuoso;

import cz.cvut.kbss.jopa.exceptions.AttributeModificationForbiddenException;
import cz.cvut.kbss.jopa.test.GenericSubclass;
import cz.cvut.kbss.jopa.test.OWLClassM;
import cz.cvut.kbss.jopa.test.OWLClassX;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class AttributesTest extends IntegrationTestBase {

    @Test
    void updatingLexicalFormAttributeThrowsAttributeModificationForbiddenException() throws Exception {
        final String key = Generators.generateUri().toString();
        final Axiom<NamedResource> classAssertion = new AxiomImpl<>(NamedResource.create(key),
                Assertion.createClassAssertion(false), new Value<>(NamedResource.create(
                Vocabulary.C_OWL_CLASS_M)));
        when(connectionMock.find(any(AxiomDescriptor.class))).thenReturn(Collections.singletonList(classAssertion));
        em.getTransaction().begin();
        final OWLClassM m = em.find(OWLClassM.class, key);
        assertNotNull(m);
        assertThrows(AttributeModificationForbiddenException.class, () -> m.setLexicalForm("test"));
    }

    @Test
    void savingEntityWithAnnotationPropertyMappedToObjectsConvertsValuesToCorrectTypes() throws Exception {
        final OWLClassX instance = new OWLClassX();
        final URI uri = Generators.generateUri();
        final Set<Object> annotationValues = new HashSet<>(Arrays.asList(1, "Two", uri));
        instance.setUri(Generators.generateUri());
        instance.setObjectAnnotation(annotationValues);
        em.getTransaction().begin();
        em.persist(instance);
        em.getTransaction().commit();

        final ArgumentCaptor<AxiomValueDescriptor> captor = ArgumentCaptor.forClass(AxiomValueDescriptor.class);
        verify(connectionMock).persist(captor.capture());
        final AxiomValueDescriptor descriptor = captor.getValue();
        final Assertion assertion = Assertion
                .createAnnotationPropertyAssertion(URI.create(Vocabulary.P_X_OBJECT_ATTRIBUTE), false);
        assertTrue(descriptor.getAssertions().contains(assertion));
        final List<Value<?>> values = descriptor.getAssertionValues(assertion);
        final Set<Object> rawValues = values.stream().map(Value::getValue).collect(Collectors.toSet());
        assertTrue(rawValues.contains(1));
        assertTrue(rawValues.contains("Two"));
        assertTrue(rawValues.contains(NamedResource.create(uri)));
    }

    @Test
    void savingEntityWithAnnotationPropertyMappedToObjectsConvertsAxiomValuesToCorrectTypes() throws Exception {
        final URI uri = Generators.generateUri();
        final NamedResource individual = NamedResource.create(uri);
        final Axiom<NamedResource> classAssertion = new AxiomImpl<>(individual, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_X)));
        final Assertion annotationAssertion = Assertion
                .createAnnotationPropertyAssertion(URI.create(Vocabulary.P_X_OBJECT_ATTRIBUTE), false);
        final Axiom<Integer> annAssertionOne = new AxiomImpl<>(individual, annotationAssertion, new Value<>(1));
        final Axiom<String> annAssertionTwo = new AxiomImpl<>(individual, annotationAssertion, new Value<>("Two"));
        final Axiom<NamedResource> annAssertionThree = new AxiomImpl<>(individual, annotationAssertion,
                new Value<>(NamedResource.create(uri)));
        when(connectionMock.find(any(AxiomDescriptor.class)))
                .thenReturn(Arrays.asList(classAssertion, annAssertionOne, annAssertionTwo, annAssertionThree));

        em.getTransaction().begin();
        final OWLClassX result = em.find(OWLClassX.class, uri);
        assertNotNull(result);
        assertThat(result.getObjectAnnotation(), hasItem(1));
        assertThat(result.getObjectAnnotation(), hasItem("Two"));
        assertThat(result.getObjectAnnotation(), hasItem(uri));
    }

    @Test
    void loadingEntitySupportsAbstractGenericSuperclass() throws Exception {
        final NamedResource subject = NamedResource.create(Generators.generateUri());
        final NamedResource a = NamedResource.create(Generators.generateUri());
        when(connectionMock.find(any(AxiomDescriptor.class))).thenAnswer(inv -> {
            final AxiomDescriptor des = inv.getArgument(0);
            if (subject.equals(des.getSubject())) {
                return List.of(
                        new AxiomImpl<>(subject, Assertion.createClassAssertion(false), new Value<>(NamedResource.create(URI.create(Vocabulary.CLASS_IRI_BASE + "GenericSubclass")))),
                        new AxiomImpl<>(subject, Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.ATTRIBUTE_IRI_BASE + "genericValue"), false), new Value<>(a))
                );
            } else {
                return List.of(
                        new AxiomImpl<>(a, Assertion.createClassAssertion(false), new Value<>(NamedResource.create(URI.create(Vocabulary.C_OWL_CLASS_A)))),
                        new AxiomImpl<>(a, Assertion.createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false), new Value<>(new LangString("Test", TestEnvironment.PERSISTENCE_LANGUAGE)))
                );
            }
        });
        em.getTransaction().begin();
        final GenericSubclass result = em.find(GenericSubclass.class, subject.getIdentifier());
        assertNotNull(result);
        assertEquals(1, result.getGenericValue().size());
        assertEquals(a.getIdentifier(), result.getGenericValue().iterator().next().getUri());
    }
}
