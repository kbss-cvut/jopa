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
package cz.cvut.kbss.jopa.oom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;

import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

public class EntityConstructorPluralAttributesTest {

    private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityC");
    private static final NamedResource SUBJECT = NamedResource.create(PK);

    private static Map<URI, OWLClassA> listContent = initListContent();
    private static URI firstListElem;

    @Mock
    private ObjectOntologyMapperImpl mapperMock;

    private MetamodelMocks metamodelMocks;

    private ListAttribute<OWLClassC, OWLClassA> simpleListMock;

    private Assertion hasSimpleListAssertion;

    private Descriptor descriptor;

    private EntityConstructor constructor;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(mapperMock.getConfiguration()).thenReturn(new Configuration(Collections.emptyMap()));
        this.metamodelMocks = new MetamodelMocks();
        this.simpleListMock = metamodelMocks.forOwlClassC().simpleListAtt();
        this.descriptor = new EntityDescriptor();
        final URI simpleListProperty = URI.create(OWLClassC.getSimpleListField()
                                                           .getAnnotation(OWLObjectProperty.class).iri());
        this.hasSimpleListAssertion = Assertion
                .createObjectPropertyAssertion(simpleListProperty, simpleListMock.isInferred());

        this.constructor = new EntityConstructor(mapperMock);
    }

    @Test
    public void reconstructsEntityWithSimpleList() throws Exception {
        final Collection<Axiom<?>> axioms = initAxiomsForC();
        prepareMapperMockForSimpleListLoad();

        final OWLClassC res = constructor
                .reconstructEntity(PK, metamodelMocks.forOwlClassC().entityType(), descriptor, axioms);

        assertNotNull(res);
        assertNotNull(res.getSimpleList());
        assertEquals(listContent.size(), res.getSimpleList().size());
        assertTrue(res.getSimpleList().containsAll(listContent.values()));
        verify(mapperMock).loadSimpleList(any(SimpleListDescriptor.class));
    }

    private void prepareMapperMockForSimpleListLoad() throws Exception {
        for (Entry<URI, OWLClassA> e : listContent.entrySet()) {
            when(
                    mapperMock.getEntityFromCacheOrOntology(OWLClassA.class, e.getKey(),
                            descriptor.getAttributeDescriptor(simpleListMock))).thenReturn(
                    e.getValue());
        }
        final Collection<Axiom<NamedResource>> listAxioms = initSimpleListAxioms();
        when(mapperMock.loadSimpleList(any(SimpleListDescriptor.class))).thenReturn(listAxioms);
    }

    private Collection<Axiom<?>> initAxiomsForC() throws Exception {
        final Collection<Axiom<?>> axioms = new HashSet<>();
        axioms.add(new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false), new Value<>(
                URI.create(OWLClassC.getClassIri()))));
        axioms.add(new AxiomImpl<>(SUBJECT, hasSimpleListAssertion, new Value<>(firstListElem)));
        return axioms;
    }

    private Collection<Axiom<NamedResource>> initSimpleListAxioms() throws Exception {
        final URI nextElemProperty = simpleListMock.getOWLObjectPropertyHasNextIRI().toURI();
        final Collection<Axiom<NamedResource>> axioms = new ArrayList<>(listContent.size());
        boolean first = true;
        URI previous = null;
        for (URI key : listContent.keySet()) {
            final Axiom<NamedResource> ax;
            if (first) {
                ax = new AxiomImpl<>(NamedResource.create(PK), hasSimpleListAssertion,
                        new Value<>(NamedResource.create(key)));
                first = false;
            } else {
                ax = new AxiomImpl<>(NamedResource.create(previous),
                        Assertion.createObjectPropertyAssertion(nextElemProperty,
                                simpleListMock.isInferred()), new Value<>(NamedResource.create(key)));
            }
            previous = key;
            axioms.add(ax);
        }
        return axioms;
    }

    @Test
    public void setsSimpleListLazilyLoadedFieldValue() throws Exception {
        final Collection<Axiom<?>> axioms = Collections.singleton(new AxiomImpl<>(
                SUBJECT, hasSimpleListAssertion, new Value<>(firstListElem)));
        prepareMapperMockForSimpleListLoad();

        final OWLClassC c = new OWLClassC();
        c.setUri(PK);
        assertNull(c.getSimpleList());
        constructor.setFieldValue(c, OWLClassC.getSimpleListField(), axioms, metamodelMocks.forOwlClassC().entityType(),
                descriptor);
        assertNotNull(c.getSimpleList());
        assertEquals(listContent.size(), c.getSimpleList().size());
        assertTrue(c.getSimpleList().containsAll(listContent.values()));
        verify(mapperMock).loadSimpleList(any(SimpleListDescriptor.class));
    }

    private static Map<URI, OWLClassA> initListContent() {
        final Map<URI, OWLClassA> map = new HashMap<>();
        for (int i = 0; i < 10; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA_" + i));
            a.setStringAttribute("stringAtt" + i);
            map.put(a.getUri(), a);
            if (i == 0) {
                firstListElem = a.getUri();
            }
        }
        return map;
    }
}
