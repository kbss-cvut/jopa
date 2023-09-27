/**
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.MappedSuperclass;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.PrePersist;
import cz.cvut.kbss.jopa.oom.exceptions.UnpersistedChangeException;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassF;
import cz.cvut.kbss.jopa.test.OWLClassJ;
import cz.cvut.kbss.jopa.test.OWLClassR;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anyOf;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
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
    void readingInstanceReferenceWithoutCorrectTypeResultsInNullAddedToPluralAttribute() throws OntoDriverException {
        final URI owner = Generators.generateUri();
        initAxiomsForNullReferenceLoad(owner);
        final OWLClassJ result = em.find(OWLClassJ.class, owner);
        assertNotNull(result);
        assertThat(result.getOwlClassA(), anyOf(nullValue(), empty()));
    }

    private void initAxiomsForNullReferenceLoad(URI owner) throws OntoDriverException {
        final NamedResource ownerResource = NamedResource.create(owner);
        final Assertion classAssertion = Assertion.createClassAssertion(false);
        final NamedResource reference = NamedResource.create(Generators.generateUri());
        final Assertion opAssertion = Assertion
                .createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_OWL_CLASS_A), false);
        final AxiomDescriptor fDesc = new AxiomDescriptor(ownerResource);
        fDesc.addAssertion(classAssertion);
        when(connectionMock.find(fDesc))
                .thenReturn(Collections.singletonList(new AxiomImpl<>(ownerResource, classAssertion,
                        new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_J)))));
        final AxiomDescriptor refDescriptor = new AxiomDescriptor(ownerResource);
        refDescriptor.addAssertion(opAssertion);
        when(connectionMock.find(refDescriptor)).thenReturn(
                Collections.singletonList(new AxiomImpl<>(ownerResource, opAssertion, new Value<>(reference))));
        final AxiomDescriptor aDesc = new AxiomDescriptor(reference);
        aDesc.addAssertion(classAssertion);
        aDesc.addAssertion(Assertion.createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false));
        when(connectionMock.find(aDesc)).thenReturn(Collections.emptyList());
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
    void getterOnLazyAttributeWithNullValueAfterPersistDoesNotTriggerLazyFetch() {
        final OWLClassF owner = new OWLClassF(Generators.generateUri());
        final OWLClassA a = new OWLClassA();
        owner.setSimpleSet(new HashSet<>(Collections.singletonList(a)));
        em.getTransaction().begin();
        em.persist(owner);
        final RollbackException ex = assertThrows(RollbackException.class, () -> em.getTransaction().commit());
        assertInstanceOf(UnpersistedChangeException.class, ex.getCause());
    }
}
