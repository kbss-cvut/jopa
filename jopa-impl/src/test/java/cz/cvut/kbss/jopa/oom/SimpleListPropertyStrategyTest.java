/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.OneOfEnum;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.model.metamodel.ListAttributeImpl;
import cz.cvut.kbss.jopa.vocabulary.OWL;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class SimpleListPropertyStrategyTest extends ListPropertyStrategyTestBase {

    private ListAttributeImpl<OWLClassC, OWLClassA> simpleList;

    private SimpleListPropertyStrategy<OWLClassC> strategy;

    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        this.simpleList = mocks.forOwlClassC().simpleListAtt();
        this.strategy =
                new SimpleListPropertyStrategy<>(mocks.forOwlClassC().entityType(), simpleList, descriptor, mapperMock);
        strategy.setReferenceSavingResolver(new ReferenceSavingResolver(mapperMock));
    }

    @Test
    void buildsInstanceFieldFromAxioms() {
        final Axiom<URI> ax = new AxiomImpl<>(NamedResource.create(IDENTIFIER),
                                              Assertion.createObjectPropertyAssertion(simpleList.getIRI().toURI(),
                                                                                      false), new Value<>(
                Generators.createIndividualIdentifier()));
        final List<OWLClassA> entitiesA = generateList();
        final Collection<Axiom<NamedResource>> axioms = buildAxiomsForList(simpleList, entitiesA);

        when(mapperMock.loadSimpleList(any(SimpleListDescriptor.class))).thenReturn(axioms);

        strategy.addValueFromAxiom(ax);
        final OWLClassC instance = new OWLClassC();
        instance.setUri(IDENTIFIER);
        strategy.buildInstanceFieldValue(instance);
        assertEquals(entitiesA.size(), instance.getSimpleList().size());
        for (OWLClassA a : entitiesA) {
            assertTrue(instance.getSimpleList().contains(a));
        }
    }

    private Collection<Axiom<NamedResource>> buildAxiomsForList(ListAttribute<?, ?> la, List<OWLClassA> lst) {
        final Collection<Axiom<NamedResource>> axioms = new ArrayList<>();
        URI previous = IDENTIFIER;
        for (OWLClassA item : lst) {
            final Axiom<NamedResource> a = new AxiomImpl<>(
                    NamedResource.create(previous),
                    Assertion.createObjectPropertyAssertion(la.getOWLObjectPropertyHasNextIRI().toURI(), false),
                    new Value<>(NamedResource.create(item.getUri())));
            axioms.add(a);
            when(mapperMock.getEntityFromCacheOrOntology(OWLClassA.class, item.getUri(),
                                                         descriptor.getAttributeDescriptor(simpleList))).thenReturn(
                    item);
            previous = item.getUri();
        }
        return axioms;
    }

    @Test
    void buildsInstanceFieldOfPlainIdentifiersFromAxioms() {
        final ListAttributeImpl<OWLClassP, URI> simpleList = mocks.forOwlClassP().pSimpleListAttribute();
        final SimpleListPropertyStrategy<OWLClassP> strategy =
                new SimpleListPropertyStrategy<>(mocks.forOwlClassP().entityType(), simpleList, descriptor, mapperMock);
        final Axiom<URI> ax = new AxiomImpl<>(NamedResource.create(IDENTIFIER),
                                              Assertion.createObjectPropertyAssertion(simpleList.getIRI().toURI(),
                                                                                      false), new Value<>(
                Generators.createIndividualIdentifier()));
        final List<OWLClassA> as = generateList();
        final Collection<Axiom<NamedResource>> axioms = buildAxiomsForList(simpleList, as);
        when(mapperMock.loadSimpleList(any(SimpleListDescriptor.class))).thenReturn(axioms);

        strategy.addValueFromAxiom(ax);
        final OWLClassP instance = new OWLClassP();
        instance.setUri(IDENTIFIER);
        strategy.buildInstanceFieldValue(instance);
        assertEquals(as.size(), instance.getSimpleList().size());
        for (int i = 0; i < as.size(); i++) {
            assertEquals(as.get(i).getUri(), instance.getSimpleList().get(i));
        }
    }

    @Test
    void addsValueFromAxiomAndVerifiesCorrectDescriptorWasCreated() {
        final Axiom<NamedResource> ax = new AxiomImpl<>(NamedResource.create(IDENTIFIER),
                                                        Assertion.createObjectPropertyAssertion(simpleList.getIRI()
                                                                                                          .toURI(),
                                                                                                false), new Value<>(
                NamedResource.create(Generators.createIndividualIdentifier())));
        final Collection<Axiom<NamedResource>> axioms = Collections.emptyList();
        when(mapperMock.loadSimpleList(any(SimpleListDescriptor.class)))
                .thenReturn(axioms);

        strategy.addValueFromAxiom(ax);
        final ArgumentCaptor<SimpleListDescriptor> captor = ArgumentCaptor
                .forClass(SimpleListDescriptor.class);
        verify(mapperMock).loadSimpleList(captor.capture());
        final SimpleListDescriptor res = captor.getValue();
        assertEquals(IDENTIFIER, res.getListOwner().getIdentifier());
        assertEquals(simpleList.getIRI().toURI(), res.getListProperty().getIdentifier());
        assertEquals(simpleList.getOWLObjectPropertyHasNextIRI().toURI(), res
                .getNextNode().getIdentifier());
        assertNull(res.getContext());
    }

    @Test
    void extractsListValuesForSave() throws Exception {
        final OWLClassC c = new OWLClassC(IDENTIFIER);
        c.setSimpleList(generateList());
        strategy.buildAxiomValuesFromInstance(c, builder);
        final SimpleListValueDescriptor res = listValueDescriptor();
        assertEquals(IDENTIFIER, res.getListOwner().getIdentifier());
        final Field simpleListField = OWLClassC.getSimpleListField();
        assertEquals(simpleListField.getAnnotation(OWLObjectProperty.class)
                                    .iri(), res.getListProperty().getIdentifier().toString());
        assertEquals(simpleListField.getAnnotation(Sequence.class)
                                    .hasNextPropertyIRI(), res.getNextNode().getIdentifier()
                                                              .toString());
        assertEquals(c.getSimpleList().size(), res.getValues().size());
        for (int i = 0; i < c.getSimpleList().size(); i++) {
            assertEquals(c.getSimpleList().get(i).getUri(), res.getValues()
                                                               .get(i).getIdentifier());
        }
    }

    private SimpleListValueDescriptor listValueDescriptor() throws Exception {
        final List<SimpleListValueDescriptor> descriptors = OOMTestUtils.getSimpleListValueDescriptors(builder);
        assertEquals(1, descriptors.size());
        return descriptors.get(0);
    }

    @Test
    void extractsListValuesForSaveListIsEmpty() throws Exception {
        final OWLClassC c = new OWLClassC(IDENTIFIER);
        c.setSimpleList(new ArrayList<>());
        strategy.buildAxiomValuesFromInstance(c, builder);

        final SimpleListValueDescriptor res = listValueDescriptor();
        assertEquals(IDENTIFIER, res.getListOwner().getIdentifier());
        final Field simpleListField = OWLClassC.getSimpleListField();
        assertEquals(simpleListField.getAnnotation(OWLObjectProperty.class).iri(),
                     res.getListProperty().getIdentifier().toString());
        assertEquals(simpleListField.getAnnotation(Sequence.class).hasNextPropertyIRI(),
                     res.getNextNode().getIdentifier().toString());
        assertTrue(res.getValues().isEmpty());
    }

    @Test
    void extractsListValuesForSaveListIsNull() throws Exception {
        final OWLClassC c = new OWLClassC(IDENTIFIER);
        c.setSimpleList(null);
        strategy.buildAxiomValuesFromInstance(c, builder);
        final SimpleListValueDescriptor res = listValueDescriptor();
        assertEquals(IDENTIFIER, res.getListOwner().getIdentifier());
        final Field simpleListField = OWLClassC.getSimpleListField();
        assertEquals(simpleListField.getAnnotation(OWLObjectProperty.class).iri(),
                     res.getListProperty().getIdentifier().toString());
        assertEquals(simpleListField.getAnnotation(Sequence.class).hasNextPropertyIRI(),
                     res.getNextNode().getIdentifier().toString());
        assertTrue(res.getValues().isEmpty());
    }

    @Test
    void extractListValuesSkipsNullItems() throws Exception {
        final OWLClassC c = new OWLClassC(IDENTIFIER);
        c.setSimpleList(generateList());
        setRandomListItemsToNull(c.getSimpleList());

        strategy.buildAxiomValuesFromInstance(c, builder);
        final SimpleListValueDescriptor res = listValueDescriptor();
        final List<URI> expected = c.getSimpleList().stream().filter(Objects::nonNull).map(OWLClassA::getUri)
                                    .collect(Collectors.toList());
        verifyListItems(expected, res);
    }

    @Test
    void extractValuesFromListOfPlainIdentifiersForPersist() throws Exception {
        final OWLClassP p = new OWLClassP();
        p.setUri(IDENTIFIER);
        p.setSimpleList(generateListOfIdentifiers());
        final ListAttributeImpl<OWLClassP, URI> simpleList = mocks.forOwlClassP().pSimpleListAttribute();
        final SimpleListPropertyStrategy<OWLClassP> strategy =
                new SimpleListPropertyStrategy<>(mocks.forOwlClassP().entityType(), simpleList, descriptor, mapperMock);

        strategy.buildAxiomValuesFromInstance(p, builder);
        final SimpleListValueDescriptor valueDescriptor = listValueDescriptor();
        verifyListItems(p.getSimpleList(), valueDescriptor);
    }

    @Test
    void extractValuesFromListSkipsNullItemsInListOfPlainIdentifiers() throws Exception {
        final OWLClassP p = new OWLClassP();
        p.setUri(IDENTIFIER);
        p.setSimpleList(generateListOfIdentifiers());
        setRandomListItemsToNull(p.getSimpleList());
        final List<URI> nonNulls = p.getSimpleList().stream().filter(Objects::nonNull).collect(Collectors.toList());
        final ListAttributeImpl<OWLClassP, URI> simpleList = mocks.forOwlClassP().pSimpleListAttribute();
        final SimpleListPropertyStrategy<OWLClassP> strategy =
                new SimpleListPropertyStrategy<>(mocks.forOwlClassP().entityType(), simpleList, descriptor, mapperMock);

        strategy.buildAxiomValuesFromInstance(p, builder);
        final SimpleListValueDescriptor valueDescriptor = listValueDescriptor();
        verifyListItems(nonNulls, valueDescriptor);
    }

    @Test
    void extractValuesRegistersPendingListItemsWhenListContainsUnpersistedItems() {
        final OWLClassC c = new OWLClassC(IDENTIFIER);
        c.setSimpleList(generateList());
        c.getSimpleList()
         .forEach(a -> when(mapperMock.containsEntity(OWLClassA.class, a.getUri(), descriptor)).thenReturn(false));
        strategy.buildAxiomValuesFromInstance(c, builder);
        c.getSimpleList()
         .forEach(item -> verify(mapperMock).registerPendingListReference(eq(item), any(), eq(c.getSimpleList())));
        verify(builder, never()).addSimpleListValues(any());
    }

    @Test
    void extractListValuesConvertsEnumConstantsToNamedResourcesForEnumValuedObjectProperty() throws Exception {
        final EntityType<WithEnumList> et = mock(EntityType.class);
        final Identifier id = mock(Identifier.class);
        when(id.getJavaField()).thenReturn(WithEnumList.class.getDeclaredField("uri"));
        when(et.getIdentifier()).thenReturn(id);
        final ListAttributeImpl<WithEnumList, OneOfEnum> att = initEnumListAttribute();
        final SimpleListPropertyStrategy<WithEnumList> sut =
                new SimpleListPropertyStrategy<>(et, att, descriptor, mapperMock);
        final WithEnumList instance = new WithEnumList();
        instance.uri = IDENTIFIER;
        instance.enumList = Arrays.asList(OneOfEnum.DATATYPE_PROPERTY, OneOfEnum.OBJECT_PROPERTY);

        sut.buildAxiomValuesFromInstance(instance, builder);
        final SimpleListValueDescriptor valueDescriptor = listValueDescriptor();
        assertEquals(
                Arrays.asList(NamedResource.create(OWL.DATATYPE_PROPERTY), NamedResource.create(OWL.OBJECT_PROPERTY)),
                valueDescriptor.getValues());
    }
}
