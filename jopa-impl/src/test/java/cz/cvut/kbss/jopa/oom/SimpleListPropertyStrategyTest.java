/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public class SimpleListPropertyStrategyTest extends
                                            ListPropertyStrategyTestBase {

    private ListAttribute<OWLClassC, OWLClassA> simpleList;

    private SimpleListPropertyStrategy<OWLClassC> strategy;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        super.setUp();
        this.simpleList = mocks.forOwlClassC().simpleListAtt();
        this.strategy =
                new SimpleListPropertyStrategy<>(mocks.forOwlClassC().entityType(), simpleList, descriptor, mapperMock);
        strategy.setCascadeResolver(cascadeResolverMock);
    }

    @Test
    public void buildsInstanceFieldFromAxioms() throws Exception {
        final Axiom<URI> ax = new AxiomImpl<>(NamedResource.create(PK),
                Assertion.createObjectPropertyAssertion(simpleList.getIRI().toURI(), false), new Value<>(
                URI.create("http://someSequence.org")));
        final List<OWLClassA> entitiesA = generateList();
        final Collection<Axiom<NamedResource>> axioms = buildAxiomsForList(simpleList, entitiesA);

        when(mapperMock.loadSimpleList(any(SimpleListDescriptor.class))).thenReturn(axioms);

        strategy.addValueFromAxiom(ax);
        final OWLClassC instance = new OWLClassC();
        instance.setUri(PK);
        strategy.buildInstanceFieldValue(instance);
        assertEquals(entitiesA.size(), instance.getSimpleList().size());
        for (OWLClassA a : entitiesA) {
            assertTrue(instance.getSimpleList().contains(a));
        }
    }

    private Collection<Axiom<NamedResource>> buildAxiomsForList(ListAttribute<?, ?> la, List<OWLClassA> lst) {
        final Collection<Axiom<NamedResource>> axioms = new ArrayList<>();
        URI previous = PK;
        for (OWLClassA item : lst) {
            final Axiom<NamedResource> a = new AxiomImpl<>(
                    NamedResource.create(previous),
                    Assertion.createObjectPropertyAssertion(la.getOWLObjectPropertyHasNextIRI().toURI(), false),
                    new Value<>(NamedResource.create(item.getUri())));
            axioms.add(a);
            when(
                    mapperMock.getEntityFromCacheOrOntology(OWLClassA.class,
                            item.getUri(), descriptor)).thenReturn(item);
            previous = item.getUri();
        }
        return axioms;
    }

    @Test
    public void buildsInstanceFieldOfPlainIdentifiersFromAxioms() throws Exception {
        final ListAttribute<OWLClassP, URI> simpleList = mocks.forOwlClassP().pSimpleListAttribute();
        final SimpleListPropertyStrategy<OWLClassP> strategy =
                new SimpleListPropertyStrategy<>(mocks.forOwlClassP().entityType(), simpleList, descriptor, mapperMock);
        final Axiom<URI> ax = new AxiomImpl<>(NamedResource.create(PK),
                Assertion.createObjectPropertyAssertion(simpleList.getIRI().toURI(), false), new Value<>(
                URI.create("http://someSequence.org")));
        final List<OWLClassA> as = generateList();
        final Collection<Axiom<NamedResource>> axioms = buildAxiomsForList(simpleList, as);
        when(mapperMock.loadSimpleList(any(SimpleListDescriptor.class))).thenReturn(axioms);

        strategy.addValueFromAxiom(ax);
        final OWLClassP instance = new OWLClassP();
        instance.setUri(PK);
        strategy.buildInstanceFieldValue(instance);
        assertEquals(as.size(), instance.getSimpleList().size());
        for (int i = 0; i < as.size(); i++) {
            assertEquals(as.get(i).getUri(), instance.getSimpleList().get(i));
        }
    }

    @Test
    public void addsValueFromAxiomAndVerifiesCorrectDescriptorWasCreated() {
        final Axiom<NamedResource> ax = new AxiomImpl<>(NamedResource.create(PK),
                Assertion.createObjectPropertyAssertion(simpleList.getIRI()
                                                                  .toURI(), false), new Value<>(
                NamedResource.create("http://someSequence.org")));
        final Collection<Axiom<NamedResource>> axioms = Collections.emptyList();
        when(mapperMock.loadSimpleList(any(SimpleListDescriptor.class)))
                .thenReturn(axioms);

        strategy.addValueFromAxiom(ax);
        final ArgumentCaptor<SimpleListDescriptor> captor = ArgumentCaptor
                .forClass(SimpleListDescriptor.class);
        verify(mapperMock).loadSimpleList(captor.capture());
        final SimpleListDescriptor res = captor.getValue();
        assertEquals(PK, res.getListOwner().getIdentifier());
        assertEquals(simpleList.getIRI().toURI(), res.getListProperty()
                                                     .getIdentifier());
        assertEquals(simpleList.getOWLObjectPropertyHasNextIRI().toURI(), res
                .getNextNode().getIdentifier());
        assertNull(res.getContext());
    }

    @Test
    public void extractsListValuesForSave() throws Exception {
        final OWLClassC c = new OWLClassC();
        c.setUri(PK);
        c.setSimpleList(generateList());
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        strategy.buildAxiomValuesFromInstance(c, builder);
        final List<SimpleListValueDescriptor> descriptors = OOMTestUtils.getSimpleListValueDescriptors(builder);
        assertEquals(1, descriptors.size());
        final SimpleListValueDescriptor res = descriptors.get(0);
        assertEquals(PK, res.getListOwner().getIdentifier());
        final Field simpleListField = OWLClassC.getSimpleListField();
        assertEquals(simpleListField.getAnnotation(OWLObjectProperty.class)
                                    .iri(), res.getListProperty().getIdentifier().toString());
        assertEquals(simpleListField.getAnnotation(Sequence.class)
                                    .ObjectPropertyHasNextIRI(), res.getNextNode().getIdentifier()
                                                                    .toString());
        assertEquals(c.getSimpleList().size(), res.getValues().size());
        for (int i = 0; i < c.getSimpleList().size(); i++) {
            assertEquals(c.getSimpleList().get(i).getUri(), res.getValues()
                                                               .get(i).getIdentifier());
        }
        verify(cascadeResolverMock, times(c.getSimpleList().size()))
                .resolveFieldCascading(eq(simpleList), any(Object.class),
                        eq((URI) null));
    }

    @Test
    public void extractsListValuesForSaveListIsEmpty() throws Exception {
        final OWLClassC c = new OWLClassC();
        c.setUri(PK);
        c.setSimpleList(new ArrayList<>());
        final AxiomValueGatherer builder = new AxiomValueGatherer(
                NamedResource.create(PK), null);
        strategy.buildAxiomValuesFromInstance(c, builder);
        final List<SimpleListValueDescriptor> descriptors = OOMTestUtils
                .getSimpleListValueDescriptors(builder);
        assertEquals(1, descriptors.size());
        final SimpleListValueDescriptor res = descriptors.get(0);
        assertEquals(PK, res.getListOwner().getIdentifier());
        final Field simpleListField = OWLClassC.getSimpleListField();
        assertEquals(simpleListField.getAnnotation(OWLObjectProperty.class)
                                    .iri(), res.getListProperty().getIdentifier().toString());
        assertEquals(simpleListField.getAnnotation(Sequence.class)
                                    .ObjectPropertyHasNextIRI(), res.getNextNode().getIdentifier()
                                                                    .toString());
        assertTrue(res.getValues().isEmpty());
        verify(cascadeResolverMock, never()).resolveFieldCascading(
                eq(simpleList), any(Object.class), eq((URI) null));
    }

    @Test
    public void extractsListValuesForSaveListIsNull() throws Exception {
        final OWLClassC c = new OWLClassC();
        c.setUri(PK);
        c.setSimpleList(null);
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        strategy.buildAxiomValuesFromInstance(c, builder);
        final List<SimpleListValueDescriptor> descriptors = OOMTestUtils
                .getSimpleListValueDescriptors(builder);
        assertEquals(1, descriptors.size());
        final SimpleListValueDescriptor res = descriptors.get(0);
        assertEquals(PK, res.getListOwner().getIdentifier());
        final Field simpleListField = OWLClassC.getSimpleListField();
        assertEquals(simpleListField.getAnnotation(OWLObjectProperty.class)
                                    .iri(), res.getListProperty().getIdentifier().toString());
        assertEquals(simpleListField.getAnnotation(Sequence.class)
                                    .ObjectPropertyHasNextIRI(), res.getNextNode().getIdentifier()
                                                                    .toString());
        assertTrue(res.getValues().isEmpty());
        verify(cascadeResolverMock, never()).resolveFieldCascading(
                eq(simpleList), any(Object.class), eq((URI) null));
    }

    @Test
    public void extractsValuesFromListOfPlainIdentifiersForPersist() throws Exception {
        final OWLClassP p = new OWLClassP();
        p.setUri(PK);
        p.setSimpleList(generateListOfIdentifiers());
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        final ListAttribute<OWLClassP, URI> simpleList = mocks.forOwlClassP().pSimpleListAttribute();
        final SimpleListPropertyStrategy<OWLClassP> strategy =
                new SimpleListPropertyStrategy<>(mocks.forOwlClassP().entityType(), simpleList, descriptor, mapperMock);

        strategy.buildAxiomValuesFromInstance(p, builder);
        final SimpleListValueDescriptor valueDescriptor = OOMTestUtils.getSimpleListValueDescriptors(builder).get(0);
        assertEquals(p.getSimpleList().size(), valueDescriptor.getValues().size());
        for (int i = 0; i < p.getSimpleList().size(); i++) {
            assertEquals(p.getSimpleList().get(i), valueDescriptor.getValues().get(i).getIdentifier());
        }
    }
}
