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

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassR;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.Test;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

/**
 * Test for discovered bugs and their fixes.
 */
public class BugTest extends IntegrationTestBase {

    /* Bug: using an attribute in hashCode/equals caused an infinite loop, because the BeanListenerAspect tried
     to check whether it was necessary to load the field, which caused instance lookup to use hashCode again
     */
    @Test
    public void hashCodeWithAttributeDoesNotCauseInfiniteLoop() throws Exception {
        final URI uri = Generators.generateUri();
        final String name = "Instance1";
        when(connectionMock.find(any(AxiomDescriptor.class))).thenReturn(initAxiomsForR(uri, name));

        final OWLClassR r = em.find(OWLClassR.class, uri);
        assertNotEquals(0, r.hashCode());
    }

    private Collection<Axiom<?>> initAxiomsForR(URI uri, String name) {
        final NamedResource nr = NamedResource.create(uri);
        final String typeIri = OWLClassR.class.getDeclaredAnnotation(OWLClass.class).iri();
        return Arrays.asList(new AxiomImpl<>(nr, Assertion.createClassAssertion(false),
                        new Value<>(NamedResource.create(typeIri))),
                new AxiomImpl<>(nr,
                        Assertion.createAnnotationPropertyAssertion(URI.create(CommonVocabulary.RDFS_LABEL), false),
                        new Value<>(name)));
    }

    /**
     * Bug #2.
     */
    @Test
    public void mergeDoesNotOverwriteCacheWithNonMergeInstance() throws OntoDriverException {
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
        initAxiomsForOwlClassD(NamedResource.create(d.getUri()), NamedResource.create(a.getUri()));
        initAxiomsForOWLClassA(NamedResource.create(a.getUri()),
                Assertion.createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false),
                a.getStringAttribute());

        a.setStringAttribute(null);
        em.getTransaction().begin();
        em.merge(d);
        em.getTransaction().commit();

        final OWLClassA result = em.find(OWLClassA.class, a.getUri());
        assertNotNull(result);
        assertEquals(str, result.getStringAttribute());
    }

    private void initAxiomsForOwlClassD(NamedResource subject, NamedResource owlClassA) throws OntoDriverException {
        final List<Axiom<?>> axioms = new ArrayList<>();
        final Axiom<?> classAssertion = new AxiomImpl<>(subject, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_D)));
        axioms.add(classAssertion);
        final Assertion opAssertion = Assertion
                .createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_OWL_CLASS_A), false);
        axioms.add(new AxiomImpl<>(subject, opAssertion, new Value<>(owlClassA)));
        final AxiomDescriptor desc = new AxiomDescriptor(subject);
        desc.addAssertion(Assertion.createClassAssertion(false));
        desc.addAssertion(opAssertion);
        when(connectionMock.find(desc)).thenReturn(axioms);
        when(connectionMock.contains(classAssertion, null)).thenReturn(true);
    }
}
