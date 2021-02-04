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

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.metamodel.ListAttributeImpl;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class ReferencedListPropertyStrategyTest extends ListPropertyStrategyTestBase {

    private static List<OWLClassA> list;

    private ListAttributeImpl<OWLClassC, OWLClassA> refListMock;

    private ReferencedListPropertyStrategy<OWLClassC> strategy;

    @BeforeAll
    static void setUpBeforeClass() {
        list = generateList();
    }

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        super.setUp();
        this.refListMock = mocks.forOwlClassC().referencedListAtt();
        this.strategy = new ReferencedListPropertyStrategy<>(mocks.forOwlClassC().entityType(), refListMock, descriptor,
                mapperMock);
        strategy.setReferenceSavingResolver(new ReferenceSavingResolver(mapperMock));
    }

    @Test
    void buildsInstanceFieldFromAxiomsIncludingNodes() throws Exception {
        final OWLClassC c = new OWLClassC(PK);
        final List<Axiom<NamedResource>> axioms = initRefListAxioms(true);
        when(mapperMock.loadReferencedList(any(ReferencedListDescriptor.class))).thenReturn(axioms);
        strategy.addValueFromAxiom(axioms.iterator().next());
        assertNull(c.getReferencedList());
        strategy.buildInstanceFieldValue(c);

        assertNotNull(c.getReferencedList());
        assertEquals(list, c.getReferencedList());
        final ArgumentCaptor<ReferencedListDescriptor> captor = ArgumentCaptor.forClass(ReferencedListDescriptor.class);
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
            final NamedResource nodeUri =
                    NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/refListNode_" + i);
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
            when(mapperMock.getEntityFromCacheOrOntology(OWLClassA.class, a.getUri(),
                    descriptor.getAttributeDescriptor(refListMock))).thenReturn(a);
            axioms.add(content);
            previous = nodeUri;
            i++;
        }
        return axioms;
    }

    /**
     * No node axioms, only content axioms.
     */
    @Test
    void buildsInstanceFieldFromAxiomsWithoutNodes() throws Exception {
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
    void buildsInstanceFieldWithPlainIdentifiers() throws Exception {
        final ListAttributeImpl<OWLClassP, URI> listAtt = mocks.forOwlClassP().pReferencedListAttribute();
        final ReferencedListPropertyStrategy<OWLClassP> strategy =
                new ReferencedListPropertyStrategy<>(mocks.forOwlClassP().entityType(), listAtt, descriptor,
                        mapperMock);
        final List<Axiom<NamedResource>> axioms = initRefListAxioms(true);
        when(mapperMock.loadReferencedList(any(ReferencedListDescriptor.class))).thenReturn(axioms);

        strategy.addValueFromAxiom(axioms.iterator().next());
        final OWLClassP p = new OWLClassP();
        p.setUri(PK);
        strategy.buildInstanceFieldValue(p);
        assertNotNull(p.getReferencedList());
        assertEquals(list.size(), p.getReferencedList().size());
        for (int i = 0; i < list.size(); i++) {
            assertEquals(list.get(i).getUri(), p.getReferencedList().get(i));
        }
    }

    @Test
    void extractsValuesIntoAxiomsForSave() throws Exception {
        final OWLClassC c = new OWLClassC(PK);
        c.setReferencedList(list);
        strategy.buildAxiomValuesFromInstance(c, builder);

        final ReferencedListValueDescriptor res = listValueDescriptor();
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
        final List<URI> expected = list.stream().map(OWLClassA::getUri).collect(Collectors.toList());
        verifyListItems(expected, res);
    }

    private ReferencedListValueDescriptor listValueDescriptor() throws Exception {
        final List<ReferencedListValueDescriptor> descriptors = OOMTestUtils.getReferencedListValueDescriptors(builder);
        assertEquals(1, descriptors.size());
        return descriptors.get(0);
    }

    @Test
    void extractValuesSkipsNullItems() throws Exception {
        final OWLClassC c = new OWLClassC(PK);
        c.setReferencedList(generateList());
        setRandomListItemsToNull(c.getReferencedList());

        strategy.buildAxiomValuesFromInstance(c, builder);
        final ReferencedListValueDescriptor res = listValueDescriptor();
        final List<URI> expected = c.getReferencedList().stream().filter(Objects::nonNull).map(OWLClassA::getUri)
                                    .collect(
                                            Collectors.toList());
        verifyListItems(expected, res);
    }

    @Test
    void extractsValuesIntoAxiomsForSaveFromEmptyList() throws Exception {
        final OWLClassC c = new OWLClassC(PK);
        c.setReferencedList(Collections.emptyList());
        strategy.buildAxiomValuesFromInstance(c, builder);

        final ReferencedListValueDescriptor res = listValueDescriptor();
        assertTrue(res.getValues().isEmpty());
    }

    @Test
    void extractsValuesIntoAxiomsForSaveFromNullList() throws Exception {
        final OWLClassC c = new OWLClassC(PK);
        c.setReferencedList(null);
        strategy.buildAxiomValuesFromInstance(c, builder);

        final ReferencedListValueDescriptor res = listValueDescriptor();
        assertTrue(res.getValues().isEmpty());
    }

    @Test
    void extractsValuesIntoAxiomsFromListOfPlainIdentifiers() throws Exception {
        final ListAttributeImpl<OWLClassP, URI> listAtt = mocks.forOwlClassP().pReferencedListAttribute();
        final ReferencedListPropertyStrategy<OWLClassP> strategy =
                new ReferencedListPropertyStrategy<>(mocks.forOwlClassP().entityType(), listAtt, descriptor,
                        mapperMock);
        final OWLClassP p = new OWLClassP();
        p.setUri(PK);
        p.setReferencedList(list.stream().map(OWLClassA::getUri).collect(Collectors.toList()));
        strategy.buildAxiomValuesFromInstance(p, builder);

        final ReferencedListValueDescriptor res = listValueDescriptor();
        p.getReferencedList().forEach(uri -> assertTrue(res.getValues().contains(NamedResource.create(uri))));
    }

    @Test
    void extractValuesFromListSkipsNullItemsInListOfPlainIdentifiers() throws Exception {
        final OWLClassP p = new OWLClassP();
        p.setUri(PK);
        p.setReferencedList(generateListOfIdentifiers());
        setRandomListItemsToNull(p.getReferencedList());
        final List<URI> nonNulls = p.getReferencedList().stream().filter(Objects::nonNull).collect(Collectors.toList());
        final ListAttributeImpl<OWLClassP, URI> refList = mocks.forOwlClassP().pReferencedListAttribute();
        final ReferencedListPropertyStrategy<OWLClassP> strategy =
                new ReferencedListPropertyStrategy<>(mocks.forOwlClassP().entityType(), refList, descriptor,
                        mapperMock);

        strategy.buildAxiomValuesFromInstance(p, builder);
        final ReferencedListValueDescriptor valueDescriptor = listValueDescriptor();
        verifyListItems(nonNulls, valueDescriptor);
    }

    @Test
    void extractValuesRegistersPendingListItemsWhenListContainsUnpersistedItems() {
        final OWLClassC c = new OWLClassC(PK);
        c.setReferencedList(generateList());
        c.getReferencedList()
         .forEach(a -> when(mapperMock.containsEntity(OWLClassA.class, a.getUri(), descriptor)).thenReturn(false));
        strategy.buildAxiomValuesFromInstance(c, builder);
        c.getReferencedList()
         .forEach(item -> verify(mapperMock).registerPendingListReference(eq(item), any(), eq(c.getReferencedList())));
        verify(builder, never()).addReferencedListValues(any());
    }
}
