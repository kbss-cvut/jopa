/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
import java.util.Collections;
import java.util.List;

import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

public class ReferencedListPropertyStrategyTest extends ListPropertyStrategyTestBase {

    private static List<OWLClassA> list;

    private ListAttribute<OWLClassC, OWLClassA> refListMock;

    private ReferencedListPropertyStrategy<OWLClassC> strategy;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        list = generateList();
    }

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        super.setUp();
        this.refListMock = mocks.forOwlClassC().referencedListAtt();
        this.strategy = new ReferencedListPropertyStrategy<>(mocks.forOwlClassC().entityType(), refListMock, descriptor,
                mapperMock);
        strategy.setCascadeResolver(cascadeResolverMock);
    }

    @Test
    public void buildsInstanceFieldFromAxiomsIncludingNodes() throws Exception {
        final OWLClassC c = new OWLClassC(PK);
        final List<Axiom<NamedResource>> axioms = initRefListAxioms(true);
        when(mapperMock.loadReferencedList(any(ReferencedListDescriptor.class))).thenReturn(axioms);
        strategy.addValueFromAxiom(axioms.iterator().next());
        assertNull(c.getReferencedList());
        strategy.buildInstanceFieldValue(c);

        assertNotNull(c.getReferencedList());
        assertEquals(list, c.getReferencedList());
        final ArgumentCaptor<ReferencedListDescriptor> captor = ArgumentCaptor
                .forClass(ReferencedListDescriptor.class);
        verify(mapperMock).loadReferencedList(captor.capture());
        final ReferencedListDescriptor listDescriptor = captor.getValue();
        assertEquals(PK, listDescriptor.getListOwner().getIdentifier());
        assertEquals(refListMock.getIRI().toURI(), listDescriptor.getListProperty().getIdentifier());
        assertEquals(refListMock.getOWLObjectPropertyHasNextIRI().toURI(), listDescriptor.getNextNode()
                                                                                         .getIdentifier());
        assertEquals(refListMock.getOWLPropertyHasContentsIRI().toURI(), listDescriptor
                .getNodeContent().getIdentifier());
    }

    private List<Axiom<NamedResource>> initRefListAxioms(boolean includeNodes) throws Exception {
        final List<Axiom<NamedResource>> axioms = new ArrayList<>();
        NamedResource previous = NamedResource.create(PK);
        int i = 0;
        for (OWLClassA a : list) {
            final NamedResource nodeUri = NamedResource
                    .create("http://krizik.felk.cvut.cz/ontologies/jopa/refListNode_" + i);
            if (includeNodes) {
                final Axiom<NamedResource> node;
                if (i == 0) {
                    node = new AxiomImpl<>(previous, Assertion.createObjectPropertyAssertion(
                            URI.create(OWLClassC.getRefListField()
                                                .getAnnotation(OWLObjectProperty.class).iri()),
                            refListMock.isInferred()), new Value<>(nodeUri));
                } else {
                    node = new AxiomImpl<>(
                            previous,
                            Assertion.createObjectPropertyAssertion(refListMock
                                    .getOWLObjectPropertyHasNextIRI().toURI(), refListMock.isInferred()),
                            new Value<>(nodeUri));
                }
                axioms.add(node);
            }
            final Axiom<NamedResource> content = new AxiomImpl<>(nodeUri,
                    Assertion.createObjectPropertyAssertion(refListMock.getOWLPropertyHasContentsIRI()
                                                                       .toURI(), refListMock.isInferred()),
                    new Value<>(NamedResource.create(a.getUri())));
            when(mapperMock.getEntityFromCacheOrOntology(OWLClassA.class, a.getUri(), descriptor))
                    .thenReturn(a);
            axioms.add(content);
            previous = nodeUri;
            i++;
        }
        return axioms;
    }

    /**
     * No node axioms, only content axioms.
     *
     * @throws Exception
     */
    @Test
    public void buildsInstanceFieldFromAxiomsWithoutNodes() throws Exception {
        final OWLClassC c = new OWLClassC(PK);
        final List<Axiom<NamedResource>> axioms = initRefListAxioms(false);
        when(mapperMock.loadReferencedList(any(ReferencedListDescriptor.class))).thenReturn(axioms);
        strategy.addValueFromAxiom(axioms.iterator().next());
        assertNull(c.getReferencedList());
        strategy.buildInstanceFieldValue(c);

        assertNotNull(c.getReferencedList());
        assertEquals(list, c.getReferencedList());
    }

    @Test
    public void extractsValuesIntoAxiomsForSave() throws Exception {
        final OWLClassC c = new OWLClassC(PK);
        c.setReferencedList(list);
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK),
                descriptor.getContext());
        strategy.buildAxiomValuesFromInstance(c, builder);

        final List<ReferencedListValueDescriptor> descriptors = OOMTestUtils
                .getReferencedListValueDescriptors(builder);
        assertEquals(1, descriptors.size());
        final ReferencedListValueDescriptor res = descriptors.get(0);
        assertEquals(res.getListOwner(), NamedResource.create(PK));
        assertEquals(
                res.getListProperty(),
                Assertion.createObjectPropertyAssertion(
                        URI.create(OWLClassC.getRefListField()
                                            .getAnnotation(OWLObjectProperty.class).iri()),
                        refListMock.isInferred()));
        assertEquals(res.getNextNode(), Assertion.createObjectPropertyAssertion(refListMock
                .getOWLObjectPropertyHasNextIRI().toURI(), refListMock.isInferred()));
        assertEquals(res.getNodeContent(), Assertion.createObjectPropertyAssertion(refListMock
                .getOWLPropertyHasContentsIRI().toURI(), refListMock.isInferred()));
        assertEquals(list.size(), res.getValues().size());
        for (OWLClassA a : list) {
            assertTrue(res.getValues().contains(NamedResource.create(a.getUri())));
        }
    }

    @Test
    public void extractsValuesIntoAxiomsForSaveFromEmptyList() throws Exception {
        final OWLClassC c = new OWLClassC(PK);
        c.setReferencedList(Collections.<OWLClassA>emptyList());
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK),
                descriptor.getContext());
        strategy.buildAxiomValuesFromInstance(c, builder);

        final List<ReferencedListValueDescriptor> descriptors = OOMTestUtils
                .getReferencedListValueDescriptors(builder);
        assertEquals(1, descriptors.size());
        final ReferencedListValueDescriptor res = descriptors.get(0);
        assertTrue(res.getValues().isEmpty());
    }

    @Test
    public void extractsValuesIntoAxiomsForSaveFromNullList() throws Exception {
        final OWLClassC c = new OWLClassC(PK);
        c.setReferencedList(null);
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK),
                descriptor.getContext());
        strategy.buildAxiomValuesFromInstance(c, builder);

        final List<ReferencedListValueDescriptor> descriptors = OOMTestUtils
                .getReferencedListValueDescriptors(builder);
        assertEquals(1, descriptors.size());
        final ReferencedListValueDescriptor res = descriptors.get(0);
        assertTrue(res.getValues().isEmpty());
    }
}
