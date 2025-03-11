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

import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.MappedSuperclass;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.PrePersist;
import cz.cvut.kbss.jopa.oom.exception.UnpersistedChangeException;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassF;
import cz.cvut.kbss.jopa.test.OWLClassJ;
import cz.cvut.kbss.jopa.test.OWLClassL;
import cz.cvut.kbss.jopa.test.OWLClassO;
import cz.cvut.kbss.jopa.test.OWLClassR;
import cz.cvut.kbss.jopa.test.OWLClassS;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.utils.JOPALazyUtils;
import cz.cvut.kbss.jopa.vocabulary.DC;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test for discovered bugs and their fixes.
 */
@ExtendWith(MockitoExtension.class)
class BugTest extends IntegrationTestBase {

    /* Bug: using an attribute in hashCode/equals caused an infinite loop, because the BeanListenerAspect tried
     to check whether it was necessary to load the field, which caused instance lookup to use hashCode again
     */
    @Test
    void hashCodeWithAttributeDoesNotCauseInfiniteLoop() throws Exception {
        final URI uri = Generators.generateUri();
        when(connectionMock.find(any())).thenReturn(initAxiomsForR(uri));

        final OWLClassR r = em.find(OWLClassR.class, uri);
        assertNotEquals(0, r.hashCode());
    }

    private Collection<Axiom<?>> initAxiomsForR(URI uri) {
        final NamedResource nr = NamedResource.create(uri);
        final String typeIri = OWLClassR.class.getDeclaredAnnotation(OWLClass.class).iri();
        return Arrays.asList(new AxiomImpl<>(nr, Assertion.createClassAssertion(false),
                        new Value<>(NamedResource.create(typeIri))),
                new AxiomImpl<>(nr,
                        Assertion.createAnnotationPropertyAssertion(URI.create(RDFS.LABEL), false),
                        new Value<>("Instance1")));
    }

    /**
     * Bug #2.
     */
    @Test
    void mergeDoesNotOverwriteCacheWithNonMergeInstance() throws OntoDriverException {
        final OWLClassD d = new OWLClassD(Generators.generateUri());
        final OWLClassA a = new OWLClassA(Generators.generateUri());
        d.setOwlClassA(a);
        final String str = "StringValue";
        a.setStringAttribute(str);
        em.getTransaction().begin();
        em.persist(d);
        em.persist(a);
        em.getTransaction().commit();
        em.clear();
        initAxiomsForOwlClassD(NamedResource.create(d.getUri()));

        a.setStringAttribute(null);
        em.getTransaction().begin();
        em.merge(d);
        em.getTransaction().commit();

        final OWLClassA result = em.find(OWLClassA.class, a.getUri());
        assertNotNull(result);
        assertEquals(str, result.getStringAttribute());
    }

    private void initAxiomsForOwlClassD(NamedResource subject) throws OntoDriverException {
        final Axiom<?> classAssertion = new AxiomImpl<>(subject, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_D)));
        doReturn(true).when(connectionMock).contains(classAssertion, Collections.emptySet());
    }

    /**
     * When a property value (instance reference) points to an individual which cannot be loaded as the target type
     * entity, nothing should be added into a plural attribute collection. The bug caused {@code null} to be added.
     */
    @Test
    void readingInstanceReferenceWithoutCorrectTypeDoesNotAddAnythingToPluralAttribute() throws OntoDriverException {
        final URI owner = Generators.generateUri();
        initAxiomsForNullReferenceLoad(owner);
        final OWLClassJ result = em.find(OWLClassJ.class, owner);
        assertNotNull(result);
        assertTrue(result.getOwlClassA().isEmpty());
    }

    private void initAxiomsForNullReferenceLoad(URI owner) throws OntoDriverException {
        final NamedResource ownerResource = NamedResource.create(owner);
        final Assertion classAssertion = Assertion.createClassAssertion(false);
        final Assertion opAssertion = Assertion
                .createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_OWL_CLASS_A), false);
        final AxiomDescriptor fDesc = new AxiomDescriptor(ownerResource);
        fDesc.addAssertion(classAssertion);
        fDesc.addAssertion(opAssertion);
        when(connectionMock.find(fDesc))
                .thenReturn(Collections.singletonList(new AxiomImpl<>(ownerResource, classAssertion,
                        new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_J)))));
    }

    /**
     * Bug #49
     */
    @Test
    void parentLifecycleCallbacksAreInvokedForEntity() {
        em.getTransaction().begin();
        final ChildWithCallback instance = new ChildWithCallback();
        instance.uri = Generators.generateUri();
        instance.label = "Test";
        em.persist(instance);
        assertTrue(instance.callbackInvoked);
        assertTrue(instance.childCallbackInvoked);
    }

    @MappedSuperclass
    public static class ParentWithCallback {
        transient boolean callbackInvoked;

        @Id
        URI uri;

        @PrePersist
        void prePersistParent() {
            this.callbackInvoked = true;
        }
    }

    @OWLClass(iri = Vocabulary.CLASS_IRI_BASE + "ChildWithCallback")
    public static class ChildWithCallback extends ParentWithCallback {

        private transient boolean childCallbackInvoked;

        @OWLAnnotationProperty(iri = RDFS.LABEL)
        private String label;

        @PrePersist
        void prePersistChild() {
            this.childCallbackInvoked = true;
        }
    }

    @Test
    void exceptionInCommitIsWrappedInRollbackException() {
        final OWLClassF owner = new OWLClassF(Generators.generateUri());
        final OWLClassA a = new OWLClassA();
        owner.setSimpleSet(new HashSet<>(Collections.singletonList(a)));
        em.getTransaction().begin();
        em.persist(owner);
        final RollbackException ex = assertThrows(RollbackException.class, () -> em.getTransaction().commit());
        assertInstanceOf(UnpersistedChangeException.class, ex.getCause());
    }

    @Test
    void triggeringLazyLoadingMultipleTimesDoesNotLeadToStackOverflow() throws Exception {
        final OWLClassF owner = new OWLClassF(Generators.generateUri());
        final OWLClassA a = new OWLClassA(Generators.generateUri());
        a.setStringAttribute("Does not matter");
        owner.setSimpleSet(Set.of(a));
        initOWLClassFAxioms(owner);
        initAxiomsForOWLClassA(NamedResource.create(a.getUri()), a.getStringAttribute(), false);

        em.getTransaction().begin();
        final OWLClassF instance = em.find(OWLClassF.class, owner.getUri());
        assertNotNull(instance);
        final Set<OWLClassA> proxy = instance.getSimpleSet();
        assertFalse(JOPALazyUtils.isLoaded(proxy));
        // Triggers lazy loading
        assertFalse(proxy.isEmpty());
        instance.setSimpleSet(proxy);
        assertFalse(proxy.isEmpty());
    }

    private void initOWLClassFAxioms(OWLClassF instance) throws Exception {
        final NamedResource subject = NamedResource.create(instance.getUri());
        final List<Axiom<?>> axioms = new ArrayList<>();
        final Axiom<?> classAssertion = new AxiomImpl<>(subject, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_F)));
        final Assertion aSetAssertion = Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_F_HAS_SIMPLE_SET), false);
        axioms.add(classAssertion);
        final List<AxiomImpl<NamedResource>> aSetAxioms = instance.getSimpleSet().stream()
                                                                  .map(a -> new AxiomImpl<>(subject, aSetAssertion, new Value<>(NamedResource.create(a.getUri()))))
                                                                  .toList();
        axioms.addAll(aSetAxioms);
        final AxiomDescriptor entityDesc = new AxiomDescriptor(subject);
        entityDesc.addAssertion(Assertion.createClassAssertion(false));
        entityDesc.addAssertion(aSetAssertion);
        entityDesc.addAssertion(Assertion.createDataPropertyAssertion(URI.create(Vocabulary.P_F_STRING_ATTRIBUTE), true));
        doReturn(axioms).when(connectionMock).find(entityDesc);

        final AxiomDescriptor setDesc = new AxiomDescriptor(subject);
        setDesc.addAssertion(aSetAssertion);
        doReturn(aSetAxioms).when(connectionMock).find(setDesc);
    }

    /**
     * Bug #248
     */
    @Test
    void cascadeMergeOnLazyLoadingProxyDoesNothing() throws Exception {
        final OWLClassO owner = new OWLClassO(Generators.generateUri());
        final OWLClassE reference = new OWLClassE();
        reference.setUri(Generators.generateUri());
        owner.setOwlClassE(reference);
        final Assertion classAssertion = Assertion.createClassAssertion(false);
        final Assertion eAssertion = Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_O_SINGLE_E_ATTRIBUTE), false);
        final Assertion eSetAssertion = Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_O_SET_OF_E_ATTRIBUTE), false);
        final NamedResource ownerSubject = NamedResource.create(owner.getUri());
        final AxiomDescriptor ownerDesc = new AxiomDescriptor(ownerSubject);
        ownerDesc.addAssertion(classAssertion);
        ownerDesc.addAssertion(eAssertion);
        ownerDesc.addAssertion(eSetAssertion);
        final List<Axiom<?>> ownerAxioms = List.of(
                new AxiomImpl<>(NamedResource.create(owner.getUri()), classAssertion, new Value<>(URI.create(Vocabulary.C_OWL_CLASS_O))),
                new AxiomImpl<>(NamedResource.create(owner.getUri()), eAssertion, new Value<>(NamedResource.create(reference.getUri())))
        );
        when(connectionMock.find(ownerDesc)).thenReturn(ownerAxioms);

        em.getTransaction().begin();
        final OWLClassO subject = em.find(OWLClassO.class, owner.getUri());
        em.merge(subject);
        verify(connectionMock, never()).update(any(AxiomValueDescriptor.class));
    }

    /**
     * Bug #252
     */
    @Test
    void isInferredTriggersLazyLoadingForLazyLoadingProxy() throws Exception {
        final OWLClassL owner = new OWLClassL(Generators.generateUri());
        final OWLClassA ref = Generators.generateOwlClassA();
        final NamedResource subject = NamedResource.create(owner.getUri());
        final Assertion classAssertion = Assertion.createClassAssertion(false);
        final Assertion hasAAssertion = Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.p_l_singleOwlClassAAttribute), false);
        final AxiomDescriptor ownerDesc = new AxiomDescriptor(subject);
        ownerDesc.addAssertion(classAssertion);
        ownerDesc.addAssertion(hasAAssertion);
        ownerDesc.addAssertion(Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.p_l_simpleListAttribute), false));
        ownerDesc.addAssertion(Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.p_l_referencedListAttribute), false));
        ownerDesc.addAssertion(Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.p_l_aSetAttribute), false));
        final List<Axiom<?>> ownerAxioms = List.of(
                new AxiomImpl<>(subject, classAssertion, new Value<>(URI.create(Vocabulary.C_OWL_CLASS_L))),
                new AxiomImpl<>(subject, hasAAssertion, new Value<>(NamedResource.create(ref.getUri())))
        );
        when(connectionMock.find(ownerDesc)).thenReturn(ownerAxioms);
        final AxiomDescriptor singleAAttDesc = new AxiomDescriptor(subject);
        singleAAttDesc.addAssertion(hasAAssertion);
        when(connectionMock.find(singleAAttDesc)).thenReturn(List.of(new AxiomImpl<>(subject, hasAAssertion, new Value<>(NamedResource.create(ref.getUri())))));
        initAxiomsForOWLClassA(NamedResource.create(ref.getUri()), ref.getStringAttribute(), false);

        em.getTransaction().begin();
        final OWLClassL instance = em.find(OWLClassL.class, owner.getUri());
        assertInstanceOf(LazyLoadingProxy.class, instance.getSingleA());
        assertFalse(em.isInferred(instance, em.getMetamodel().entity(OWLClassL.class)
                                              .getAttribute("singleA"), instance.getSingleA()));
        final ArgumentCaptor<Axiom<?>> axiomCaptor = ArgumentCaptor.forClass(Axiom.class);
        verify(connectionMock).isInferred(axiomCaptor.capture(), eq(Collections.emptySet()));
        assertEquals(new Value<>(NamedResource.create(ref.getUri())), axiomCaptor.getValue().getValue());
        final ArgumentCaptor<AxiomDescriptor> captor = ArgumentCaptor.forClass(AxiomDescriptor.class);
        verify(connectionMock, atLeastOnce()).find(captor.capture());
        assertTrue(captor.getAllValues().stream()
                         .anyMatch(ad -> ad.getSubject().equals(NamedResource.create(ref.getUri()))));
    }

    /**
     * Bug #265
     */
    @Test
    void updateRegistersChangesByInheritedPreUpdate() throws Exception {
        final OWLClassS entity = new OWLClassS();
        entity.setUri(Generators.generateUri());
        final NamedResource subject = NamedResource.create(entity.getUri());
        final List<Axiom<?>> axioms = new ArrayList<>();
        final Axiom<?> classAssertion = new AxiomImpl<>(subject, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_S)));
        axioms.add(classAssertion);
        final AxiomDescriptor desc = new AxiomDescriptor(subject);
        desc.addAssertion(Assertion.createClassAssertion(false));
        desc.addAssertion(Assertion.createAnnotationPropertyAssertion(URI.create(RDFS.LABEL), false));
        desc.addAssertion(Assertion.createDataPropertyAssertion(URI.create(DC.Terms.DESCRIPTION), false));
        desc.addAssertion(Assertion.createDataPropertyAssertion(URI.create(DC.Terms.MODIFIED), false));
        doReturn(axioms).when(connectionMock).find(desc);
        doReturn(true).when(connectionMock).contains(classAssertion, Collections.emptySet());
        final Types types = mock(Types.class);
        when(connectionMock.types()).thenReturn(types);
        when(types.getTypes(subject, Set.of(), false)).thenReturn(Set.of(
                new AxiomImpl<>(subject, Assertion.createClassAssertion(false), new Value<>(URI.create(Vocabulary.C_OWL_CLASS_S)))
        ));

        entity.setName("Updated name");
        em.getTransaction().begin();
        em.merge(entity);
        em.getTransaction().commit();
        final ArgumentCaptor<AxiomValueDescriptor> captor = ArgumentCaptor.forClass(AxiomValueDescriptor.class);
        verify(connectionMock, times(2)).update(captor.capture());
        final List<AxiomValueDescriptor> value = captor.getAllValues();
        assertTrue(value.stream().anyMatch(avd -> avd.getAssertions()
                                                     .contains(Assertion.createDataPropertyAssertion(URI.create(DC.Terms.MODIFIED), false))));
        assertTrue(value.stream().anyMatch(avd -> avd.getAssertions()
                                                     .contains(Assertion.createAnnotationPropertyAssertion(URI.create(RDFS.LABEL), false))));
    }

    /**
     * Bug #264
     */
    @Test
    void oomHandlesMapOfInstances() throws Exception {
        final Properties properties = mock(Properties.class);
        when(connectionMock.properties()).thenReturn(properties);
        final OWLClassB entity = new OWLClassB();
        entity.setUri(Generators.generateUri());
        entity.setProperties(Map.of(Vocabulary.P_Q_STRING_ATTRIBUTE, Set.of("Test")));
        em.getTransaction().begin();
        em.persist(entity);
        em.getTransaction().commit();

        final ArgumentCaptor<AxiomValueDescriptor> captor = ArgumentCaptor.forClass(AxiomValueDescriptor.class);
        verify(connectionMock).persist(captor.capture());
        final AxiomValueDescriptor value = captor.getValue();
        assertThat(value.getAssertions(), hasItem(Assertion.createClassAssertion(false)));
        verify(properties).addProperties(NamedResource.create(entity.getUri()), null,
                Map.of(Assertion.createPropertyAssertion(URI.create(Vocabulary.P_Q_STRING_ATTRIBUTE), false), Set.of(new Value<>("Test"))));
    }
}
