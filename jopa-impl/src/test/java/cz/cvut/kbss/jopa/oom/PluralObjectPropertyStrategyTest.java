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
import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.OneOfEnum;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.descriptors.ObjectPropertyCollectionDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractPluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.oom.converter.ObjectOneOfEnumConverter;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingSetProxy;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.vocabulary.OWL;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.MalformedURLException;
import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasItems;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PluralObjectPropertyStrategyTest {

    private static final URI ID = Generators.createIndividualIdentifier();

    @Mock
    private EntityMappingHelper mapperMock;

    private final Descriptor descriptor = new EntityDescriptor();

    private MetamodelMocks mocks;

    @BeforeEach
    void setUp() throws Exception {
        this.mocks = new MetamodelMocks();
    }

    @Test
    void addValueFromAxiomLoadsInstanceAndAddsItIntoAttributeCollection() {
        final PluralObjectPropertyStrategy<?, OWLClassJ> sut = strategy();
        final OWLClassJ instance = new OWLClassJ(ID);
        final URI aReference = Generators.createIndividualIdentifier();
        final OWLClassA aInstance = new OWLClassA(aReference);
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(aReference), any(Descriptor.class)))
                .thenReturn(aInstance);
        final Axiom<NamedResource> axiom = new AxiomImpl<>(NamedResource.create(ID),
                                                           Assertion.createObjectPropertyAssertion(
                                                                   URI.create(Vocabulary.P_HAS_A), false), new Value<>(
                NamedResource.create(aReference)));

        sut.addValueFromAxiom(axiom);
        sut.buildInstanceFieldValue(instance);
        assertNotNull(instance.getOwlClassA());
        assertEquals(1, instance.getOwlClassA().size());
        assertTrue(instance.getOwlClassA().contains(aInstance));
        verify(mapperMock).getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(aReference), any(Descriptor.class));
    }

    private PluralObjectPropertyStrategy<?, OWLClassJ> strategy() {
        return new SimpleSetPropertyStrategy<>(mocks.forOwlClassJ().entityType(), mocks.forOwlClassJ().setAttribute(),
                                               descriptor, mapperMock);
    }

    @Test
    void addValueFromAxiomDoesNothingWhenInstanceCannotBeLoaded() {
        final PluralObjectPropertyStrategy<?, OWLClassJ> sut = strategy();
        final OWLClassJ instance = new OWLClassJ(ID);
        final URI aReference = Generators.createIndividualIdentifier();
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(aReference), any(Descriptor.class)))
                .thenReturn(null);
        final Axiom<NamedResource> axiom = new AxiomImpl<>(NamedResource.create(ID),
                                                           Assertion.createObjectPropertyAssertion(
                                                                   URI.create(Vocabulary.P_HAS_A), false), new Value<>(
                NamedResource.create(aReference)));

        sut.addValueFromAxiom(axiom);
        sut.buildInstanceFieldValue(instance);
        assertNull(instance.getOwlClassA());
        verify(mapperMock).getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(aReference), any(Descriptor.class));
    }

    @Test
    void addValueFromAxiomGetsAttributeDescriptorFromEntityDescriptorForLoading() {
        final EntityDescriptor aDescriptor = new EntityDescriptor(Generators.createIndividualIdentifier());
        descriptor.addAttributeDescriptor(mocks.forOwlClassJ().setAttribute(), aDescriptor);
        final PluralObjectPropertyStrategy<?, OWLClassJ> sut = strategy();
        final URI aReference = Generators.createIndividualIdentifier();
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(aReference), any(Descriptor.class)))
                .thenReturn(null);
        final Axiom<NamedResource> axiom = new AxiomImpl<>(NamedResource.create(ID),
                                                           Assertion.createObjectPropertyAssertion(
                                                                   URI.create(Vocabulary.P_HAS_A), false), new Value<>(
                NamedResource.create(aReference)));

        sut.addValueFromAxiom(axiom);
        verify(mapperMock).getEntityFromCacheOrOntology(OWLClassA.class, aReference, aDescriptor);
    }

    @Test
    void buildAxiomValuesFromInstanceChecksForReferenceExistenceUsingTargetReferenceContext() {
        final Descriptor aDescriptor = new ObjectPropertyCollectionDescriptor(Generators.createIndividualIdentifier(),
                                                                              mocks.forOwlClassJ().setAttribute());
        descriptor.addAttributeDescriptor(mocks.forOwlClassJ().setAttribute(), aDescriptor);
        final PluralObjectPropertyStrategy<?, OWLClassJ> sut = strategy();
        final ReferenceSavingResolver resolverMock = mock(ReferenceSavingResolver.class);
        sut.setReferenceSavingResolver(resolverMock);
        final OWLClassJ instance = new OWLClassJ(ID);
        final OWLClassA aInstance = Generators.generateOwlClassAInstance();
        instance.setOwlClassA(Collections.singleton(aInstance));
        sut.buildAxiomValuesFromInstance(instance, new AxiomValueGatherer(NamedResource.create(ID), null));
        verify(resolverMock).shouldSaveReferenceToItem(aInstance, aDescriptor.getContexts());
    }

    @Test
    void buildAxiomValuesFromInstanceChecksForListItemReferenceExistenceUsingTargetReferenceContext() {
        final Descriptor aDescriptor = new ObjectPropertyCollectionDescriptor(Generators.createIndividualIdentifier(),
                                                                              mocks.forOwlClassC().simpleListAtt());
        descriptor.addAttributeDescriptor(mocks.forOwlClassC().simpleListAtt(), aDescriptor);
        final SimpleListPropertyStrategy<OWLClassC> sut =
                new SimpleListPropertyStrategy<>(mocks.forOwlClassC().entityType(),
                                                 mocks.forOwlClassC().simpleListAtt(), descriptor, mapperMock);
        final ReferenceSavingResolver resolverMock = mock(ReferenceSavingResolver.class);
        sut.setReferenceSavingResolver(resolverMock);
        final OWLClassC instance = new OWLClassC(ID);
        final OWLClassA aInstance = Generators.generateOwlClassAInstance();
        instance.setSimpleList(Collections.singletonList(aInstance));
        sut.buildAxiomValuesFromInstance(instance, new AxiomValueGatherer(NamedResource.create(ID), null));
        verify(resolverMock).shouldSaveReferenceToItem(aInstance, aDescriptor.getContexts());
    }

    @Test
    void buildAxiomsFromInstanceReturnsAxiomsCorrespondingToAttributeValue() {
        final OWLClassC instance = new OWLClassC(ID);
        instance.setSimpleList(Generators.generateInstances(5));
        final SimpleListPropertyStrategy<OWLClassC> sut =
                new SimpleListPropertyStrategy<>(mocks.forOwlClassC().entityType(),
                                                 mocks.forOwlClassC().simpleListAtt(), descriptor, mapperMock);
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(mocks.forOwlClassA().entityType());

        final Set<Axiom<?>> result = sut.buildAxiomsFromInstance(instance);
        assertEquals(instance.getSimpleList().size(), result.size());
        final NamedResource subject = NamedResource.create(ID);
        final Assertion assertion =
                Assertion.createObjectPropertyAssertion(mocks.forOwlClassC().simpleListAtt().getIRI()
                                                             .toURI(), false);
        instance.getSimpleList().forEach(
                a -> assertThat(result, hasItem(new AxiomImpl<>(subject, assertion,
                                                                new Value<>(NamedResource.create(a.getUri()))))));
    }

    @Test
    void buildAxiomsFromInstanceReturnsAxiomsWithIdentifiersWhenAttributeValueIsCollectionOfIdentifiers() {
        final SimpleSetPropertyStrategy<OWLClassP> sut =
                new SimpleSetPropertyStrategy<>(mocks.forOwlClassP().entityType(),
                                                mocks.forOwlClassP().pUrlsAttribute(), descriptor, mapperMock);
        final OWLClassP p = new OWLClassP();
        p.setUri(Generators.createIndividualIdentifier());
        p.setIndividualUrls(IntStream.range(0, 5).mapToObj(i -> {
            try {
                return Generators.createIndividualIdentifier().toURL();
            } catch (MalformedURLException e) {
                throw new IllegalArgumentException(e);
            }
        }).collect(Collectors.toSet()));

        final Set<Axiom<?>> result = sut.buildAxiomsFromInstance(p);
        assertEquals(p.getIndividualUrls().size(), result.size());
        final NamedResource subject = NamedResource.create(p.getUri());
        final Assertion assertion =
                Assertion.createObjectPropertyAssertion(mocks.forOwlClassP().pUrlsAttribute().getIRI().toURI(), false);
        p.getIndividualUrls().forEach(u -> assertThat(result, hasItem(new AxiomImpl<>(subject, assertion, new Value<>(
                NamedResource.create(u.toString()))))));
    }

    @Test
    void buildAxiomsFromInstanceReturnsEmptyCollectionWhenAttributeValueIsEmpty() {
        final OWLClassC instance = new OWLClassC(ID);
        instance.setSimpleList(null);
        final SimpleListPropertyStrategy<OWLClassC> sut =
                new SimpleListPropertyStrategy<>(mocks.forOwlClassC().entityType(),
                                                 mocks.forOwlClassC().simpleListAtt(), descriptor, mapperMock);

        final Set<Axiom<?>> result = sut.buildAxiomsFromInstance(instance);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void addValueFromAxiomConvertsNamedResourceToEnumConstantForEnumValuedObjectProperty() throws Exception {
        final SimpleSetPropertyStrategy<EntityWithPluralObjectPropertyEnum> sut = initEnumAttributeStrategy();
        final NamedResource value = NamedResource.create(OWL.ANNOTATION_PROPERTY);

        sut.addValueFromAxiom(new AxiomImpl<>(NamedResource.create(ID), Assertion.createObjectPropertyAssertion(
                URI.create(Vocabulary.p_m_objectOneOfEnumAttribute), false), new Value<>(value)));
        final EntityWithPluralObjectPropertyEnum instance = new EntityWithPluralObjectPropertyEnum();
        sut.buildInstanceFieldValue(instance);
        assertEquals(Collections.singleton(OneOfEnum.ANNOTATION_PROPERTY), instance.enumSet);
    }

    private SimpleSetPropertyStrategy<EntityWithPluralObjectPropertyEnum> initEnumAttributeStrategy() throws NoSuchFieldException {
        final EntityType<EntityWithPluralObjectPropertyEnum> et = mock(EntityType.class);
        final AbstractPluralAttribute<EntityWithPluralObjectPropertyEnum, Set<OneOfEnum>, OneOfEnum> att =
                mock(AbstractPluralAttribute.class);
        when(att.getBindableJavaType()).thenReturn(OneOfEnum.class);
        when(att.getJavaField()).thenReturn(EntityWithPluralObjectPropertyEnum.class.getDeclaredField("enumSet"));
        when(att.getConverter()).thenReturn(new ObjectOneOfEnumConverter<>(OneOfEnum.class));
        when(att.getCollectionType()).thenReturn(CollectionType.SET);
        when(att.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_objectOneOfEnumAttribute));
        final Identifier id = mock(Identifier.class);
        when(id.getJavaField()).thenReturn(EntityWithPluralObjectPropertyEnum.class.getDeclaredField("uri"));
        when(et.getIdentifier()).thenReturn(id);
        return new SimpleSetPropertyStrategy<>(et, att, descriptor, mapperMock);
    }

    static class EntityWithPluralObjectPropertyEnum {

        @Id
        URI uri;

        Set<OneOfEnum> enumSet;
    }

    @Test
    void buildAxiomsFromInstanceConvertsEnumConstantsToNamedResourcesForEnumValuedObjectProperty() throws Exception {
        final SimpleSetPropertyStrategy<EntityWithPluralObjectPropertyEnum> sut = initEnumAttributeStrategy();
        final EntityWithPluralObjectPropertyEnum instance = new EntityWithPluralObjectPropertyEnum();
        instance.uri = ID;
        instance.enumSet = new HashSet<>(Arrays.asList(OneOfEnum.ANNOTATION_PROPERTY, OneOfEnum.OBJECT_PROPERTY));

        final Set<Axiom<?>> result = sut.buildAxiomsFromInstance(instance);
        assertEquals(instance.enumSet.size(), result.size());
        final Assertion assertion =
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.p_m_objectOneOfEnumAttribute), false);
        assertThat(result, hasItems(
                new AxiomImpl<>(NamedResource.create(ID), assertion,
                                new Value<>(NamedResource.create(OWL.ANNOTATION_PROPERTY))),
                new AxiomImpl<>(NamedResource.create(ID), assertion,
                                new Value<>(NamedResource.create(OWL.OBJECT_PROPERTY)))
        ));
    }

    @Test
    void buildInstanceFieldValueSetsInstanceFieldValueToNullWhenNoAxiomsWereAdded() {
        // This ensures lazy loading proxy is replaced with null when lazy loaded field loading is triggered
        final SimpleSetPropertyStrategy<OWLClassJ> sut = new SimpleSetPropertyStrategy<>(mocks.forOwlClassJ().entityType(), mocks.forOwlClassJ()
                                                                                                                                 .setAttribute(), descriptor, mapperMock);
        final OWLClassJ instance = new OWLClassJ(ID);
        instance.setOwlClassA(new LazyLoadingSetProxy<>(instance, mocks.forOwlClassJ().setAttribute(), mock(UnitOfWork.class)));
        sut.buildInstanceFieldValue(instance);
        assertNull(instance.getOwlClassA());
    }
}
