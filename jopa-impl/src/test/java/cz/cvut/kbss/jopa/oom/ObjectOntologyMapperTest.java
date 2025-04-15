/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.OWLClassS;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.oom.exception.UnpersistedChangeException;
import cz.cvut.kbss.jopa.proxy.reference.EntityReferenceProxy;
import cz.cvut.kbss.jopa.proxy.reference.EntityReferenceProxyGenerator;
import cz.cvut.kbss.jopa.sessions.AbstractUnitOfWork;
import cz.cvut.kbss.jopa.sessions.cache.CacheManager;
import cz.cvut.kbss.jopa.sessions.cache.Descriptors;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.util.LoadStateDescriptorRegistry;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Lists;
import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anySet;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ObjectOntologyMapperTest {

    private static final URI IDENTIFIER = Generators.createIndividualIdentifier();

    private static OWLClassA entityA;
    private static Descriptor aDescriptor;
    private static Set<String> aTypes;
    private static AxiomDescriptor axiomDescriptor;

    @Mock
    private AbstractUnitOfWork uowMock;

    @Mock
    private Connection connectionMock;

    @Mock
    private MetamodelImpl metamodelMock;
    @Mock
    private CacheManager cacheMock;

    @Mock
    private AxiomDescriptorFactory descriptorFactoryMock;

    @Mock
    private EntityConstructor entityConstructorMock;
    @Mock
    private EntityDeconstructor entityDeconstructorMock;

    private final LoadStateDescriptorRegistry loadStateRegistry = new LoadStateDescriptorRegistry(Object::toString);

    private MetamodelMocks mocks;
    private EntityType<OWLClassA> etAMock;
    private LoadingParameters<OWLClassA> loadingParameters;

    private ObjectOntologyMapperImpl mapper;

    @BeforeAll
    static void setUpBeforeClass() {
        entityA = new OWLClassA(IDENTIFIER);
        entityA.setStringAttribute("SomeStringAttribute");
        aTypes = new HashSet<>();
        aTypes.add("http://krizik.felk.cvut.cz/ontologies/entityU");
        aTypes.add("http://krizik.felk.cvut.cz/ontologies/entityV");
        aDescriptor = new EntityDescriptor();
        axiomDescriptor = new AxiomDescriptor(NamedResource.create(IDENTIFIER));
    }

    @BeforeEach
    void setUp() throws Exception {
        when(uowMock.getMetamodel()).thenReturn(metamodelMock);
        when(uowMock.getLiveObjectCache()).thenReturn(cacheMock);
        when(uowMock.getConfiguration()).thenReturn(new Configuration(Collections.emptyMap()));
        when(uowMock.getLoadStateRegistry()).thenReturn(loadStateRegistry);
        this.loadingParameters = new LoadingParameters<>(OWLClassA.class, IDENTIFIER, aDescriptor);
        this.mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        this.etAMock = mocks.forOwlClassA().entityType();
        when(descriptorFactoryMock.createForEntityLoading(loadingParameters, etAMock)).thenReturn(axiomDescriptor);
        when(descriptorFactoryMock.createForFieldLoading(IDENTIFIER, mocks.forOwlClassA().typesSpec(),
                aDescriptor, mocks.forOwlClassA().entityType())).thenReturn(
                axiomDescriptor);
        entityA.setTypes(null);
        this.mapper = new ObjectOntologyMapperImpl(uowMock, connectionMock);
        TestEnvironmentUtils.setMock(mapper,
                ObjectOntologyMapperImpl.class.getDeclaredField("descriptorFactory"),
                descriptorFactoryMock);
        TestEnvironmentUtils.setMock(mapper,
                ObjectOntologyMapperImpl.class.getDeclaredField("entityBuilder"),
                entityConstructorMock);
        TestEnvironmentUtils.setMock(mapper,
                ObjectOntologyMapperImpl.class.getDeclaredField("entityBreaker"),
                entityDeconstructorMock);
    }

    private Collection<Axiom<?>> getAxiomsForEntityA() {
        final List<Axiom<?>> res = new ArrayList<>();
        final NamedResource identifier = NamedResource.create(IDENTIFIER);
        res.add(new AxiomImpl<>(identifier, Assertion.createClassAssertion(false),
                new Value<Object>(NamedResource.create(Vocabulary.c_OwlClassA))));
        res.add(new AxiomImpl<>(identifier,
                Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_a_stringAttribute),
                        false),
                new Value<>("stringAttribute")));
        return res;
    }

    @Test
    void testLoadFieldValue() throws Exception {
        final Field typesField = OWLClassA.getTypesField();
        typesField.setAccessible(true);
        assertNull(typesField.get(entityA));
        final Collection<Axiom<?>> axiomsForA = getAxiomsForEntityA();
        when(connectionMock.find(axiomDescriptor)).thenReturn(axiomsForA);
        doAnswer(invocation -> {
            final OWLClassA a = (OWLClassA) invocation.getArguments()[0];
            final FieldSpecification<?, ?> types = (FieldSpecification<?, ?>) invocation.getArguments()[1];
            EntityPropertiesUtils.setFieldValue(types.getJavaField(), a, aTypes);
            return null;
        }).when(entityConstructorMock)
          .setFieldValue(entityA, mocks.forOwlClassA().typesSpec(), axiomsForA, etAMock, aDescriptor);
        mapper.loadFieldValue(entityA, mocks.forOwlClassA().typesSpec(), aDescriptor);
        assertNotNull(typesField.get(entityA));
        assertEquals(aTypes, entityA.getTypes());
        verify(connectionMock).find(axiomDescriptor);
        verify(entityConstructorMock).setFieldValue(entityA, mocks.forOwlClassA()
                                                                  .typesSpec(), axiomsForA, etAMock, aDescriptor);
    }

    @Test
    void testLoadFieldValueStorageException() throws Exception {
        when(connectionMock.find(axiomDescriptor)).thenThrow(new OntoDriverException());
        final Field typesField = OWLClassA.getTypesField();
        typesField.setAccessible(true);
        assertNull(typesField.get(entityA));
        assertThrows(StorageAccessException.class, () -> mapper.loadFieldValue(entityA, mocks.forOwlClassA()
                                                                                             .typesSpec(), aDescriptor));
        verify(entityConstructorMock, never()).setFieldValue(any(),
                eq(mocks.forOwlClassA().typesSpec()), any(), any(),
                any());
    }

    @Test
    void testPersistEntity() {
        final AxiomValueGatherer madMock = mock(AxiomValueGatherer.class);
        when(entityDeconstructorMock.mapEntityToAxioms(IDENTIFIER, entityA, etAMock, aDescriptor))
                .thenReturn(madMock);
        mapper.persistEntity(IDENTIFIER, entityA, aDescriptor);
        verify(madMock).persist(connectionMock);
    }

    @Test
    void testGetEntityFromCacheOrOntologyFromCache() {
        when(cacheMock.contains(OWLClassA.class, IDENTIFIER, aDescriptor)).thenReturn(Boolean.TRUE);
        when(cacheMock.get(OWLClassA.class, IDENTIFIER, aDescriptor)).thenReturn(entityA);
        final LoadStateDescriptor<OWLClassA> loadStateDescriptor = new LoadStateDescriptor<>(entityA, mocks.forOwlClassA()
                                                                                                           .entityType(), LoadState.UNKNOWN);
        doReturn(loadStateDescriptor).when(cacheMock).getLoadStateDescriptor(entityA);
        final OWLClassA res = mapper.getEntityFromCacheOrOntology(OWLClassA.class, IDENTIFIER, aDescriptor);
        assertNotNull(res);
        assertSame(entityA, res);
        verify(cacheMock).get(OWLClassA.class, IDENTIFIER, aDescriptor);
    }

    @Test
    void testGetEntityFromCacheOrOntologyFromRegisteredInstances() {
        when(cacheMock.contains(OWLClassA.class, IDENTIFIER, null)).thenReturn(Boolean.FALSE);
        mapper.registerInstance(IDENTIFIER, entityA);
        final OWLClassA res = mapper.getEntityFromCacheOrOntology(OWLClassA.class, IDENTIFIER,
                aDescriptor);
        assertNotNull(res);
        assertSame(entityA, res);
    }

    @Test
    void getEntityFromCacheOrOntologyLoadsEntityWhenItIsNotInCache() throws Exception {
        when(cacheMock.contains(OWLClassA.class, IDENTIFIER, null)).thenReturn(Boolean.FALSE);
        final Field instanceLoaderField = ObjectOntologyMapperImpl.class.getDeclaredField("defaultInstanceLoader");
        instanceLoaderField.setAccessible(true);
        EntityInstanceLoader loader = (EntityInstanceLoader) instanceLoaderField.get(mapper);
        loader = spy(loader);
        instanceLoaderField.set(mapper, loader);
        doReturn(entityA).when(loader).loadEntity(loadingParameters);
        final LoadStateDescriptor<OWLClassA> loadStateDescriptor = new LoadStateDescriptor<>(entityA, mocks.forOwlClassA()
                                                                                                           .entityType(), LoadState.UNKNOWN);
        loadStateRegistry.put(entityA, loadStateDescriptor);
        final OWLClassA res = mapper.getEntityFromCacheOrOntology(OWLClassA.class, IDENTIFIER, aDescriptor);
        assertSame(entityA, res);
        verify(loader).loadEntity(loadingParameters);
    }

    @Test
    void testRegisterInstance() throws Exception {
        final Field regField = mapper.getClass().getDeclaredField("instanceRegistry");
        regField.setAccessible(true);
        final Map<URI, Object> reg = (Map<URI, Object>) regField.get(mapper);
        assertFalse(reg.containsKey(IDENTIFIER));
        mapper.registerInstance(IDENTIFIER, entityA);
        assertTrue(reg.containsKey(IDENTIFIER));
    }

    @Test
    void removesEntityWithEmptyDescriptor() throws Exception {
        final LoadingParameters<OWLClassA> p = new LoadingParameters<>(OWLClassA.class, IDENTIFIER, aDescriptor, true);
        when(descriptorFactoryMock.createForEntityLoading(p, etAMock)).thenReturn(axiomDescriptor);
        mapper.removeEntity(IDENTIFIER, OWLClassA.class, aDescriptor);
        verify(descriptorFactoryMock).createForEntityLoading(p, etAMock);
        verify(connectionMock).remove(axiomDescriptor);
    }

    @Test
    void throwsStorageAccessWhenRemovingEntity() throws Exception {
        doThrow(OntoDriverException.class).when(connectionMock).remove(any());
        assertThrows(StorageAccessException.class, () -> mapper.removeEntity(IDENTIFIER, OWLClassA.class, aDescriptor));
    }

    @Test
    void updatesFieldValueInTheOntology() {
        final AxiomValueGatherer axiomBuilderMock = mock(AxiomValueGatherer.class);
        when(entityDeconstructorMock.mapFieldToAxioms(IDENTIFIER, entityA,
                mocks.forOwlClassA().stringAttribute(), etAMock,
                aDescriptor)).thenReturn(
                axiomBuilderMock);
        mapper.updateFieldValue(entityA, mocks.forOwlClassA().stringAttribute(), aDescriptor);
        verify(entityDeconstructorMock).mapFieldToAxioms(IDENTIFIER, entityA,
                mocks.forOwlClassA().stringAttribute(), etAMock, aDescriptor);
        verify(axiomBuilderMock).update(connectionMock);
    }

    @Test
    void removeEntityCreatesDescriptorForRemovalOfAllEntityAttributes() {
        when(descriptorFactoryMock
                .createForEntityLoading(new LoadingParameters<>(OWLClassA.class, IDENTIFIER, aDescriptor, true),
                        etAMock)).thenReturn(axiomDescriptor);
        mapper.removeEntity(IDENTIFIER, OWLClassA.class, aDescriptor);
        final ArgumentCaptor<LoadingParameters<OWLClassA>> captor = ArgumentCaptor.forClass(LoadingParameters.class);
        verify(descriptorFactoryMock).createForEntityLoading(captor.capture(), eq(etAMock));
        final LoadingParameters<OWLClassA> p = captor.getValue();
        assertTrue(p.isForceEager());
    }

    @Test
    void containsEntityThrowsStorageAccessExceptionWhenOntoDriverExceptionIsThrown() throws Exception {
        final String message = "OntoDriver exception was thrown";
        when(connectionMock.contains(any(Axiom.class), anySet())).thenThrow(new OntoDriverException(message));

        final StorageAccessException ex = assertThrows(StorageAccessException.class,
                () -> mapper.containsEntity(OWLClassA.class, IDENTIFIER,
                        aDescriptor));
        assertThat(ex.getMessage(), containsString(message));
        verify(connectionMock).contains(
                new AxiomImpl<>(NamedResource.create(IDENTIFIER), Assertion.createClassAssertion(false),
                        new Value<>(NamedResource.create(OWLClassA.getClassIri()))), Collections.emptySet());
    }

    @Test
    void loadSimpleListThrowsStorageAccessExceptionWhenOntoDriverExceptionIsThrown() throws Exception {
        final String message = "OntoDriver exception was thrown";
        final Lists listsMock = mock(Lists.class);
        when(listsMock.loadSimpleList(any(SimpleListDescriptor.class))).thenThrow(new OntoDriverException(message));
        when(connectionMock.lists()).thenReturn(listsMock);

        final SimpleListDescriptor listDescriptorMock = mock(SimpleListDescriptor.class);
        final StorageAccessException ex = assertThrows(StorageAccessException.class,
                () -> mapper.loadSimpleList(listDescriptorMock));
        assertThat(ex.getMessage(), containsString(message));
        verify(listsMock).loadSimpleList(listDescriptorMock);
    }

    @Test
    void getReferencedListThrowsStorageAccessExceptionWhenOntoDriverExceptionIsThrown() throws Exception {
        final String message = "OntoDriver exception was thrown";
        final Lists listsMock = mock(Lists.class);
        when(listsMock.loadReferencedList(any(ReferencedListDescriptor.class)))
                .thenThrow(new OntoDriverException(message));
        when(connectionMock.lists()).thenReturn(listsMock);

        final ReferencedListDescriptor listDescriptorMock = mock(ReferencedListDescriptor.class);
        final StorageAccessException ex = assertThrows(StorageAccessException.class,
                () -> mapper.loadReferencedList(listDescriptorMock));
        assertThat(ex.getMessage(), containsString(message));
        verify(listsMock).loadReferencedList(listDescriptorMock);
    }

    @Test
    void usesTwoStepInstanceLoaderForLoadingInstanceOfEntityWithSubtypes() throws Exception {
        final Field twoStepLoaderField = ObjectOntologyMapperImpl.class.getDeclaredField("twoStepInstanceLoader");
        twoStepLoaderField.setAccessible(true);
        EntityInstanceLoader twoStepLoader = (EntityInstanceLoader) twoStepLoaderField.get(mapper);
        twoStepLoader = spy(twoStepLoader);
        twoStepLoaderField.set(mapper, twoStepLoader);
        final OWLClassS entity = new OWLClassR();
        final LoadingParameters<OWLClassS> loadingParameters = new LoadingParameters<>(OWLClassS.class, IDENTIFIER,
                aDescriptor);
        doReturn(entity).when(twoStepLoader).loadEntity(loadingParameters);
        final LoadStateDescriptor<OWLClassS> loadStateDescriptor = new LoadStateDescriptor<>(entity, mocks.forOwlClassS()
                                                                                                          .entityType(), LoadState.UNKNOWN);
        loadStateRegistry.put(entity, loadStateDescriptor);

        final OWLClassS result = mapper.loadEntity(loadingParameters);
        assertSame(entity, result);
        verify(twoStepLoader).loadEntity(loadingParameters);
    }

    @Test
    void loadEntityLoadsInstanceFromCacheWhenItIsPresentThere() throws Exception {
        when(cacheMock.contains(OWLClassA.class, IDENTIFIER, loadingParameters.getDescriptor())).thenReturn(true);
        when(cacheMock.get(OWLClassA.class, IDENTIFIER, loadingParameters.getDescriptor())).thenReturn(entityA);
        final LoadStateDescriptor<OWLClassA> loadStateDescriptor = new LoadStateDescriptor<>(entityA, mocks.forOwlClassA()
                                                                                                           .entityType(), LoadState.UNKNOWN);
        doReturn(loadStateDescriptor).when(cacheMock).getLoadStateDescriptor(entityA);

        final OWLClassA result = mapper.loadEntity(loadingParameters);
        assertSame(entityA, result);
        verify(cacheMock).get(OWLClassA.class, IDENTIFIER, loadingParameters.getDescriptor());
        verify(connectionMock, never()).find(any(AxiomDescriptor.class));
    }

    @Test
    void loadEntityDeterminesConcreteEntityTypeAndLoadsItFromCacheWhenItIsPresentThere() throws Exception {
        final OWLClassR entity = new OWLClassR();
        entity.setUri(IDENTIFIER);
        when(cacheMock.contains(OWLClassR.class, IDENTIFIER, aDescriptor)).thenReturn(true);
        when(cacheMock.get(OWLClassR.class, IDENTIFIER, aDescriptor)).thenReturn(entity);
        final LoadStateDescriptor<OWLClassR> loadStateDescriptor = new LoadStateDescriptor<>(entity, mocks.forOwlClassR()
                                                                                                          .entityType(), LoadState.UNKNOWN);
        doReturn(loadStateDescriptor).when(cacheMock).getLoadStateDescriptor(entity);
        final Types typesMock = mock(Types.class);
        final NamedResource individual = NamedResource.create(IDENTIFIER);
        final URI typeUri = URI.create(Vocabulary.C_OWLClassR);
        when(typesMock.getTypes(NamedResource.create(IDENTIFIER), Collections.emptySet(), false))
                .thenReturn(Collections.singleton(
                        new AxiomImpl<>(individual, Assertion.createClassAssertion(false), new Value<>(typeUri))));
        when(connectionMock.types()).thenReturn(typesMock);

        final LoadingParameters<OWLClassS> loadingParameters = new LoadingParameters<>(OWLClassS.class, IDENTIFIER,
                aDescriptor);
        final OWLClassS result = mapper.loadEntity(loadingParameters);
        assertSame(entity, result);
        verify(cacheMock).get(OWLClassR.class, IDENTIFIER, aDescriptor);
        verify(connectionMock, never()).find(any(AxiomDescriptor.class));
    }

    @Test
    void loadEntityPutsItIntoSecondLevelCache() throws Exception {
        final Collection<Axiom<?>> axiomsForA = getAxiomsForEntityA();
        when(connectionMock.find(any(AxiomDescriptor.class))).thenReturn(axiomsForA);
        final OWLClassA result = mapper.loadEntity(loadingParameters);
        assertNotNull(result);
        verify(cacheMock).add(IDENTIFIER, result, new Descriptors(loadingParameters.getDescriptor(), loadStateRegistry.get(result)));
    }

    @Test
    void loadEntityDoesNotPutIntoSecondLevelCacheWhenBypassCache() throws Exception {
        final Collection<Axiom<?>> axiomsForA = getAxiomsForEntityA();
        when(connectionMock.find(any(AxiomDescriptor.class))).thenReturn(axiomsForA);
        loadingParameters.bypassCache();
        final OWLClassA result = mapper.loadEntity(loadingParameters);
        assertNotNull(result);
        verify(cacheMock, never()).add(IDENTIFIER, result, new Descriptors(loadingParameters.getDescriptor(), loadStateRegistry.get(result)));
    }

    @Test
    void loadEntityPutsIntoSecondLevelCacheThenEntityAndEntitiesItReferences() throws Exception {
        final Collection<Axiom<?>> axiomsForA = getAxiomsForEntityA();
        final URI identifier = Generators.createIndividualIdentifier();
        final Collection<Axiom<?>> axiomsForD = axiomsForD(identifier);
        when(connectionMock.find(any(AxiomDescriptor.class))).thenAnswer(invocationOnMock -> {
            final AxiomDescriptor arg = (AxiomDescriptor) invocationOnMock.getArguments()[0];
            if (arg.getSubject().equals(NamedResource.create(identifier))) {
                return axiomsForD;
            } else {
                return axiomsForA;
            }
        });
        final OWLClassD result = mapper.loadEntity(new LoadingParameters<>(OWLClassD.class, identifier, aDescriptor));
        assertNotNull(result);
        verify(cacheMock).add(identifier, result, new Descriptors(aDescriptor, loadStateRegistry.get(result)));
        verify(cacheMock).add(IDENTIFIER, result.getOwlClassA(), new Descriptors(aDescriptor, loadStateRegistry.get(result.getOwlClassA())));
    }

    private Collection<Axiom<?>> axiomsForD(URI identifier) {
        final NamedResource id = NamedResource.create(identifier);
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        axioms.add(new AxiomImpl<>(id, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.c_OwlClassD))));
        axioms.add(new AxiomImpl<>(id, Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false),
                new Value<Object>(NamedResource.create(IDENTIFIER))));
        return axioms;
    }

    @Test
    void loadEntitySkipsCacheWhenNothingIsFound() throws Exception {
        when(connectionMock.find(any(AxiomDescriptor.class))).thenReturn(Collections.emptyList());
        final OWLClassA result = mapper.loadEntity(loadingParameters);
        assertNull(result);
        verify(cacheMock, never()).add(any(), any(), any());
    }

    @Test
    void persistRemovesPendingAssertionsWithTargetBeingPersistedObject() throws Exception {
        final OWLClassA a = new OWLClassA();
        a.setStringAttribute("string");
        final OWLClassD d = new OWLClassD(Generators.createIndividualIdentifier());
        d.setOwlClassA(a);
        initDeconstructorMock(d, aDescriptor);
        mapper.persistEntity(d.getUri(), d, aDescriptor);
        a.setUri(Generators.createIndividualIdentifier());
        final NamedResource aIndividual = NamedResource.create(a.getUri());
        when(entityDeconstructorMock.mapEntityToAxioms(a.getUri(), a, etAMock, aDescriptor))
                .thenReturn(new AxiomValueGatherer(aIndividual, null));
        mapper.persistEntity(a.getUri(), a, aDescriptor);
        final ArgumentCaptor<AxiomValueDescriptor> captor = ArgumentCaptor.forClass(AxiomValueDescriptor.class);
        verify(connectionMock, times(3)).persist(captor.capture());
        final AxiomValueDescriptor assertionDesc = captor.getAllValues().get(2);
        assertEquals(d.getUri(), assertionDesc.getSubject().getIdentifier());
        assertEquals(1, assertionDesc.getAssertions().size());
        final Assertion assertion = assertionDesc.getAssertions().iterator().next();
        assertEquals(URI.create(Vocabulary.P_HAS_A), assertion.getIdentifier());
        assertEquals(1, assertionDesc.getAssertionValues(assertion).size());
        assertEquals(aIndividual, assertionDesc.getAssertionValues(assertion).get(0).getValue());
    }

    private void initDeconstructorMock(OWLClassD d, Descriptor descriptor) {
        when(entityDeconstructorMock
                .mapEntityToAxioms(d.getUri(), d, metamodelMock.entity(OWLClassD.class), descriptor))
                .then(invocationOnMock -> {
                    final Assertion assertion =
                            Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false);
                    final Descriptor attDescriptor =
                            descriptor.getAttributeDescriptor(mocks.forOwlClassD().owlClassAAtt());
                    mapper.registerPendingAssertion(NamedResource.create(d.getUri()), assertion, d.getOwlClassA(),
                            attDescriptor.getSingleContext().orElse(null));
                    return new AxiomValueGatherer(NamedResource.create(d.getUri()),
                            descriptor.getSingleContext().orElse(null));
                });
    }

    @Test
    void persistPersistsPendingAssertionIntoCorrectContext() throws Exception {
        final OWLClassA a = new OWLClassA();
        final OWLClassD d = new OWLClassD(Generators.createIndividualIdentifier());
        final Descriptor descriptor = new EntityDescriptor();
        final Descriptor aDescriptor = new EntityDescriptor(Generators.createIndividualIdentifier());
        descriptor.addAttributeDescriptor(mocks.forOwlClassD().owlClassAAtt(), aDescriptor);
        d.setOwlClassA(a);
        final Assertion assertion =
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false);
        initDeconstructorMock(d, descriptor);
        mapper.persistEntity(d.getUri(), d, descriptor);
        a.setUri(Generators.createIndividualIdentifier());
        final NamedResource aIndividual = NamedResource.create(a.getUri());
        when(entityDeconstructorMock.mapEntityToAxioms(a.getUri(), a, etAMock, aDescriptor))
                .thenReturn(new AxiomValueGatherer(aIndividual, null));
        mapper.persistEntity(a.getUri(), a, aDescriptor);
        final ArgumentCaptor<AxiomValueDescriptor> captor = ArgumentCaptor.forClass(AxiomValueDescriptor.class);
        verify(connectionMock, times(3)).persist(captor.capture());
        final AxiomValueDescriptor assertionDesc = captor.getAllValues().get(2);
        assertEquals(aDescriptor.getSingleContext().get(), assertionDesc.getAssertionContext(assertion));
    }

    @Test
    void checkForUnpersistedChangesThrowsPendingPersistExceptionWhenThereArePendingChanges() {
        final URI subject = Generators.createIndividualIdentifier();
        final Assertion assertion =
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false);
        mapper.checkForUnpersistedChanges();
        mapper.registerPendingAssertion(NamedResource.create(subject), assertion, entityA, null);
        final UnpersistedChangeException ex = assertThrows(UnpersistedChangeException.class,
                () -> mapper.checkForUnpersistedChanges());
        assertThat(ex.getMessage(), containsString(entityA.toString()));
    }

    @Test
    void removeEntityRemovesPendingReferenceWhenOwnerIsRemoved() throws Exception {
        final Assertion assertion =
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false);
        final Descriptor descriptor = new EntityDescriptor();
        mapper.registerPendingAssertion(NamedResource.create(IDENTIFIER), assertion, entityA, null);
        final AxiomDescriptor axiomDescriptor = new AxiomDescriptor(NamedResource.create(IDENTIFIER));
        axiomDescriptor.addAssertion(Assertion.createClassAssertion(false));
        axiomDescriptor.addAssertion(Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false));
        when(descriptorFactoryMock
                .createForEntityLoading(new LoadingParameters<>(OWLClassD.class, IDENTIFIER, descriptor, true),
                        metamodelMock.entity(OWLClassD.class))).thenReturn(axiomDescriptor);

        final PendingReferenceRegistry registry = getPendingAssertionRegistry();
        assertTrue(registry.getPendingResources().contains(entityA));
        mapper.removeEntity(IDENTIFIER, OWLClassD.class, descriptor);
        assertFalse(registry.getPendingResources().contains(entityA));
    }

    private PendingReferenceRegistry getPendingAssertionRegistry() throws Exception {
        final Field field = mapper.getClass().getDeclaredField("pendingReferences");
        field.setAccessible(true);
        return (PendingReferenceRegistry) field.get(mapper);
    }

    @Test
    void updateFieldValueRemovesPendingReference() throws Exception {
        final OWLClassD owner = new OWLClassD(IDENTIFIER);
        owner.setOwlClassA(entityA);
        final Assertion assertion =
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false);
        final Descriptor descriptor = new EntityDescriptor();
        mapper.registerPendingAssertion(NamedResource.create(IDENTIFIER), assertion, entityA, null);
        final OWLClassA differentA = new OWLClassA(Generators.createIndividualIdentifier());
        owner.setOwlClassA(differentA);
        when(entityDeconstructorMock.mapFieldToAxioms(IDENTIFIER, owner, mocks.forOwlClassD().owlClassAAtt(),
                metamodelMock.entity(OWLClassD.class), descriptor))
                .thenReturn(new AxiomValueGatherer(NamedResource.create(IDENTIFIER), null));

        mapper.updateFieldValue(owner, mocks.forOwlClassD().owlClassAAtt(), descriptor);
        final PendingReferenceRegistry registry = getPendingAssertionRegistry();
        assertFalse(registry.getPendingResources().contains(entityA));
    }

    @Test
    void persistSavesPendingSimpleListContainingPersistedInstance() throws Exception {
        final OWLClassC owner = new OWLClassC(IDENTIFIER);
        owner.setSimpleList(Generators.generateInstances(5));
        final OWLClassA pending = owner.getSimpleList().get(0);
        final Assertion listProp =
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_SIMPLE_LIST), false);
        final Assertion nextProp =
                Assertion.createObjectPropertyAssertion(URI.create(SequencesVocabulary.s_p_hasNext), false);
        final SimpleListValueDescriptor listDesc =
                new SimpleListValueDescriptor(NamedResource.create(IDENTIFIER), listProp, nextProp);
        mapper.registerPendingListReference(pending, listDesc, owner.getSimpleList());

        when(entityDeconstructorMock.mapEntityToAxioms(pending.getUri(), pending, etAMock, aDescriptor))
                .thenReturn(new AxiomValueGatherer(NamedResource.create(pending.getUri()), null));
        final Lists listsMock = mock(Lists.class);
        when(connectionMock.lists()).thenReturn(listsMock);
        mapper.persistEntity(pending.getUri(), pending, aDescriptor);
        verify(connectionMock).lists();
        verify(listsMock).updateSimpleList(any());
    }

    @Test
    void persistSavesPendingReferencedListContainingPersistedInstance() throws Exception {
        final OWLClassC owner = new OWLClassC(IDENTIFIER);
        owner.setReferencedList(Generators.generateInstances(5));
        final OWLClassA pending = owner.getReferencedList().get(Generators.randomInt(5));
        final Assertion listProp =
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_REFERENCED_LIST), false);
        final Assertion nextProp =
                Assertion.createObjectPropertyAssertion(URI.create(SequencesVocabulary.s_p_hasNext), false);
        final Assertion valueProp =
                Assertion.createObjectPropertyAssertion(URI.create(SequencesVocabulary.s_p_hasContents), false);
        final ReferencedListValueDescriptor listDesc =
                new ReferencedListValueDescriptor(NamedResource.create(IDENTIFIER), listProp, nextProp, valueProp);
        mapper.registerPendingListReference(pending, listDesc, owner.getReferencedList());

        when(entityDeconstructorMock.mapEntityToAxioms(pending.getUri(), pending, etAMock, aDescriptor))
                .thenReturn(new AxiomValueGatherer(NamedResource.create(pending.getUri()), null));
        final Lists listsMock = mock(Lists.class);
        when(connectionMock.lists()).thenReturn(listsMock);
        mapper.persistEntity(pending.getUri(), pending, aDescriptor);
        verify(connectionMock).lists();
        verify(listsMock).updateReferencedList(any());
    }

    @Test
    void getEntityFromCacheOrOntologyThrowsEntityExistsWhenObjectIsAlreadyRegisteredUnderDifferentType() {
        mapper.registerInstance(IDENTIFIER, entityA);
        assertThrows(OWLEntityExistsException.class,
                () -> mapper.getEntityFromCacheOrOntology(OWLClassB.class, IDENTIFIER, aDescriptor));
    }

    @Test
    void getReferenceReturnsReferenceObjectOfRequestedEntityTypeWithIdentifierSet() throws Exception {
        when(metamodelMock.getEntityReferenceProxy(OWLClassA.class)).thenReturn((Class) new EntityReferenceProxyGenerator().generate(OWLClassA.class));
        final OWLClassA result = mapper.getReference(loadingParameters);
        assertNotNull(result);
        assertEquals(IDENTIFIER, result.getUri());
        assertInstanceOf(EntityReferenceProxy.class, result);
        final EntityReferenceProxy<OWLClassA> proxy = (EntityReferenceProxy<OWLClassA>) result;
        assertEquals(uowMock, proxy.getPersistenceContext());
        assertEquals(aDescriptor, proxy.getDescriptor());
        assertEquals(IDENTIFIER, proxy.getIdentifier());
        assertEquals(OWLClassA.class, proxy.getType());
        verify(connectionMock, never()).find(any());
    }
}
