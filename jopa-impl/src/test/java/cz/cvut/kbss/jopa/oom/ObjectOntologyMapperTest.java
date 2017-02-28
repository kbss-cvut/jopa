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

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.oom.exceptions.UnpersistedChangeException;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Lists;
import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public class ObjectOntologyMapperTest {

    private static final URI ENTITY_PK = Generators.createIndividualIdentifier();

    private static OWLClassA entityA;
    private static Descriptor aDescriptor;
    private static Set<String> aTypes;
    private static AxiomDescriptor axiomDescriptor;

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private UnitOfWorkImpl uowMock;

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

    private EntityType<OWLClassA> etAMock;
    private LoadingParameters<OWLClassA> loadingParameters;

    private ObjectOntologyMapperImpl mapper;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        entityA = new OWLClassA();
        entityA.setUri(ENTITY_PK);
        entityA.setStringAttribute("SomeStringAttribute");
        aTypes = new HashSet<>();
        aTypes.add("http://krizik.felk.cvut.cz/ontologies/entityU");
        aTypes.add("http://krizik.felk.cvut.cz/ontologies/entityV");
        aDescriptor = new EntityDescriptor();
        axiomDescriptor = new AxiomDescriptor(NamedResource.create(ENTITY_PK));
    }

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(uowMock.getMetamodel()).thenReturn(metamodelMock);
        when(uowMock.getLiveObjectCache()).thenReturn(cacheMock);
        when(uowMock.getConfiguration()).thenReturn(new Configuration(Collections.emptyMap()));
        this.loadingParameters = new LoadingParameters<>(OWLClassA.class, ENTITY_PK, aDescriptor);
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        this.etAMock = mocks.forOwlClassA().entityType();
        when(descriptorFactoryMock.createForEntityLoading(loadingParameters, etAMock)).thenReturn(axiomDescriptor);
        when(
                descriptorFactoryMock.createForFieldLoading(ENTITY_PK, OWLClassA.getTypesField(),
                        aDescriptor, mocks.forOwlClassA().entityType())).thenReturn(axiomDescriptor);
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
        final NamedResource identifier = NamedResource.create(ENTITY_PK);
        res.add(new AxiomImpl<>(identifier, Assertion.createClassAssertion(false),
                new Value<Object>(NamedResource.create(Vocabulary.c_OwlClassA))));
        res.add(new AxiomImpl<>(identifier,
                Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_a_stringAttribute), false),
                new Value<>("stringAttribute")));
        return res;
    }

    @Test
    public void testLoadFieldValue() throws Exception {
        final Field typesField = OWLClassA.getTypesField();
        typesField.setAccessible(true);
        assertNull(typesField.get(entityA));
        final Collection<Axiom<?>> axiomsForA = getAxiomsForEntityA();
        when(connectionMock.find(axiomDescriptor)).thenReturn(axiomsForA);
        doAnswer(invocation -> {
            final OWLClassA a = (OWLClassA) invocation.getArguments()[0];
            final Field types = (Field) invocation.getArguments()[1];
            types.setAccessible(true);
            types.set(a, aTypes);
            return null;
        }).when(entityConstructorMock).setFieldValue(entityA, typesField, axiomsForA, etAMock, aDescriptor);
        mapper.loadFieldValue(entityA, typesField, aDescriptor);
        assertNotNull(typesField.get(entityA));
        assertEquals(aTypes, entityA.getTypes());
        verify(connectionMock).find(axiomDescriptor);
        verify(entityConstructorMock).setFieldValue(entityA, typesField, axiomsForA, etAMock, aDescriptor);
    }

    @Test
    public void testLoadFieldValueEmpty() throws Exception {
        final Field typesField = OWLClassA.getTypesField();
        typesField.setAccessible(true);
        assertNull(typesField.get(entityA));
        when(connectionMock.find(axiomDescriptor)).thenReturn(Collections.emptyList());
        mapper.loadFieldValue(entityA, typesField, aDescriptor);
        assertNull(typesField.get(entityA));
        verify(entityConstructorMock, never()).setFieldValue(any(), eq(typesField),
                any(), any(), eq(aDescriptor));
    }

    @Test(expected = StorageAccessException.class)
    public void testLoadFieldValueStorageException() throws Exception {
        when(connectionMock.find(axiomDescriptor)).thenThrow(new OntoDriverException());
        final Field typesField = OWLClassA.getTypesField();
        typesField.setAccessible(true);
        assertNull(typesField.get(entityA));
        try {
            mapper.loadFieldValue(entityA, typesField, aDescriptor);
            fail("This line should not have been reached.");
        } finally {
            verify(entityConstructorMock, never()).setFieldValue(any(),
                    eq(typesField), any(), any(),
                    any());
        }
    }

    @Test
    public void testPersistEntity() throws Exception {
        final AxiomValueGatherer madMock = mock(AxiomValueGatherer.class);
        when(entityDeconstructorMock.mapEntityToAxioms(ENTITY_PK, entityA, etAMock, aDescriptor))
                .thenReturn(madMock);
        mapper.persistEntity(ENTITY_PK, entityA, aDescriptor);
        verify(madMock).persist(connectionMock);
    }

    @Test
    public void testPersistEntityWithGeneratedURI() throws Exception {
        final OWLClassA a = new OWLClassA();
        final AxiomValueGatherer madMock = mock(AxiomValueGatherer.class);
        final URI generatedUri = URI.create("http://generatedUri" + System.currentTimeMillis());
        when(entityDeconstructorMock.mapEntityToAxioms(generatedUri, a, etAMock, aDescriptor))
                .thenReturn(madMock);
        when(connectionMock.generateIdentifier(etAMock.getIRI().toURI())).thenReturn(generatedUri);

        assertNull(a.getUri());
        mapper.persistEntity(null, a, aDescriptor);
        assertNotNull(a.getUri());
        verify(connectionMock).generateIdentifier(etAMock.getIRI().toURI());
        verify(madMock).persist(connectionMock);
    }

    @Test
    public void testGetEntityFromCacheOrOntologyFromCache() {
        when(cacheMock.contains(OWLClassA.class, ENTITY_PK, null)).thenReturn(Boolean.TRUE);
        when(cacheMock.get(OWLClassA.class, ENTITY_PK, null)).thenReturn(entityA);
        final OWLClassA res = mapper.getEntityFromCacheOrOntology(OWLClassA.class, ENTITY_PK,
                aDescriptor);
        assertNotNull(res);
        assertSame(entityA, res);
        verify(cacheMock).get(OWLClassA.class, ENTITY_PK, null);
    }

    @Test
    public void testGetEntityFromCacheOrOntologyFromRegisteredInstances() {
        when(cacheMock.contains(OWLClassA.class, ENTITY_PK, null)).thenReturn(Boolean.FALSE);
        mapper.registerInstance(ENTITY_PK, entityA, null);
        final OWLClassA res = mapper.getEntityFromCacheOrOntology(OWLClassA.class, ENTITY_PK,
                aDescriptor);
        assertNotNull(res);
        assertSame(entityA, res);
    }

    @Test
    public void getEntityFromCacheOrOntologyLoadsEntityWhenItIsNotInCache() throws Exception {
        when(cacheMock.contains(OWLClassA.class, ENTITY_PK, null)).thenReturn(Boolean.FALSE);
        final Field instanceLoaderField = ObjectOntologyMapperImpl.class.getDeclaredField("defaultInstanceLoader");
        instanceLoaderField.setAccessible(true);
        EntityInstanceLoader loader = (EntityInstanceLoader) instanceLoaderField.get(mapper);
        loader = spy(loader);
        instanceLoaderField.set(mapper, loader);
        doReturn(entityA).when(loader).loadEntity(loadingParameters);
        final OWLClassA res = mapper.getEntityFromCacheOrOntology(OWLClassA.class, ENTITY_PK,
                aDescriptor);
        assertSame(entityA, res);
        verify(loader).loadEntity(loadingParameters);
    }

    @Test
    public void testRegisterInstance() throws Exception {
        final URI context = URI.create("http://someNamedContext");
        final Field regField = mapper.getClass().getDeclaredField("instanceRegistry");
        regField.setAccessible(true);
        final InstanceRegistry reg = (InstanceRegistry) regField.get(mapper);
        assertFalse(reg.containsInstance(ENTITY_PK, context));
        mapper.registerInstance(ENTITY_PK, entityA, context);
        assertTrue(reg.containsInstance(ENTITY_PK, context));
    }

    @Test
    public void checksForUnpersistedChangesAndThereAreNone() {
        mapper.checkForUnpersistedChanges();
    }

    @Test(expected = UnpersistedChangeException.class)
    public void checksForUnpersistedChangesThrowsExceptionWhenThereAre() throws Exception {
        mapper.registerPendingPersist(ENTITY_PK, entityA, null);
        when(connectionMock.find(any(AxiomDescriptor.class))).thenReturn(Collections.emptyList());
        mapper.checkForUnpersistedChanges();
    }

    @Test
    public void removesEntityWithEmptyDescriptor() throws Exception {
        final LoadingParameters<OWLClassA> p = new LoadingParameters<>(OWLClassA.class, ENTITY_PK, aDescriptor, true);
        when(descriptorFactoryMock.createForEntityLoading(p, etAMock)).thenReturn(axiomDescriptor);
        mapper.removeEntity(ENTITY_PK, OWLClassA.class, aDescriptor);
        verify(descriptorFactoryMock).createForEntityLoading(p, etAMock);
        verify(connectionMock).remove(axiomDescriptor);
    }

    @Test(expected = StorageAccessException.class)
    public void throwsStorageAccessWhenRemovingEntity() throws Exception {
        doThrow(OntoDriverException.class).when(connectionMock).remove(any(AxiomDescriptor.class));
        mapper.removeEntity(ENTITY_PK, OWLClassA.class, aDescriptor);
    }

    @Test
    public void updatesFieldValueInTheOntology() throws Exception {
        final AxiomValueGatherer axiomBuilderMock = mock(AxiomValueGatherer.class);
        when(
                entityDeconstructorMock.mapFieldToAxioms(ENTITY_PK, entityA,
                        OWLClassA.getStrAttField(), etAMock, aDescriptor)).thenReturn(
                axiomBuilderMock);
        mapper.updateFieldValue(entityA, OWLClassA.getStrAttField(), aDescriptor);
        verify(entityDeconstructorMock).mapFieldToAxioms(ENTITY_PK, entityA,
                OWLClassA.getStrAttField(), etAMock, aDescriptor);
        verify(axiomBuilderMock).update(connectionMock);
    }

    @Test
    public void removeEntityCreatesDescriptorForRemovalOfAllEntityAttributes() throws Exception {
        mapper.removeEntity(ENTITY_PK, OWLClassA.class, aDescriptor);
        final ArgumentCaptor<LoadingParameters> captor = ArgumentCaptor.forClass(LoadingParameters.class);
        verify(descriptorFactoryMock).createForEntityLoading(captor.capture(), eq(etAMock));
        final LoadingParameters p = captor.getValue();
        assertTrue(p.isForceLoad());
    }

    @Test
    public void containsEntityThrowsStorageAccessExceptionWhenOntoDriverExceptionIsThrown() throws Exception {
        final String message = "OntoDriver exception was thrown";
        when(connectionMock.contains(any(Axiom.class), eq(null))).thenThrow(new OntoDriverException(message));
        thrown.expect(StorageAccessException.class);
        thrown.expectMessage(message);

        mapper.containsEntity(OWLClassA.class, ENTITY_PK, aDescriptor);
        verify(connectionMock).contains(
                new AxiomImpl<>(NamedResource.create(ENTITY_PK), Assertion.createClassAssertion(false),
                        new Value<>(NamedResource.create(OWLClassA.getClassIri()))), null);
    }

    @Test
    public void loadSimpleListThrowsStorageAccessExceptionWhenOntoDriverExceptionIsThrown() throws Exception {
        final String message = "OntoDriver exception was thrown";
        thrown.expect(StorageAccessException.class);
        thrown.expectMessage(message);
        final Lists listsMock = mock(Lists.class);
        when(listsMock.loadSimpleList(any(SimpleListDescriptor.class))).thenThrow(new OntoDriverException(message));
        when(connectionMock.lists()).thenReturn(listsMock);

        final SimpleListDescriptor listDescriptorMock = mock(SimpleListDescriptor.class);
        mapper.loadSimpleList(listDescriptorMock);
        verify(listsMock).loadSimpleList(listDescriptorMock);
    }

    @Test
    public void loadReferencedListThrowsStorageAccessExceptionWhenOntoDriverExceptionIsThrown() throws Exception {
        final String message = "OntoDriver exception was thrown";
        thrown.expect(StorageAccessException.class);
        thrown.expectMessage(message);
        final Lists listsMock = mock(Lists.class);
        when(listsMock.loadReferencedList(any(ReferencedListDescriptor.class)))
                .thenThrow(new OntoDriverException(message));
        when(connectionMock.lists()).thenReturn(listsMock);

        final ReferencedListDescriptor listDescriptorMock = mock(ReferencedListDescriptor.class);
        mapper.loadReferencedList(listDescriptorMock);
        verify(listsMock).loadReferencedList(listDescriptorMock);
    }

    @Test
    public void usesTwoStepInstanceLoaderForLoadingInstanceOfEntityWithSubtypes() throws Exception {
        final Field twoStepLoaderField = ObjectOntologyMapperImpl.class.getDeclaredField("twoStepInstanceLoader");
        twoStepLoaderField.setAccessible(true);
        EntityInstanceLoader twoStepLoader = (EntityInstanceLoader) twoStepLoaderField.get(mapper);
        twoStepLoader = spy(twoStepLoader);
        twoStepLoaderField.set(mapper, twoStepLoader);
        final OWLClassS entity = new OWLClassR();
        final LoadingParameters<OWLClassS> loadingParameters = new LoadingParameters<>(OWLClassS.class, ENTITY_PK,
                aDescriptor);
        doReturn(entity).when(twoStepLoader).loadEntity(loadingParameters);

        final OWLClassS result = mapper.loadEntity(loadingParameters);
        assertSame(entity, result);
        verify(twoStepLoader).loadEntity(loadingParameters);
    }

    @Test
    public void loadEntityLoadsInstanceFromCacheWhenItIsPresentThere() throws Exception {
        when(cacheMock.contains(OWLClassA.class, ENTITY_PK, null)).thenReturn(true);
        when(cacheMock.get(OWLClassA.class, ENTITY_PK, null)).thenReturn(entityA);

        final OWLClassA result = mapper.loadEntity(loadingParameters);
        assertSame(entityA, result);
        verify(cacheMock).get(OWLClassA.class, ENTITY_PK, null);
        verify(connectionMock, never()).find(any(AxiomDescriptor.class));
    }

    @Test
    public void loadEntityDeterminesConcreteEntityTypeAndLoadsItFromCacheWhenItIsPresentThere() throws Exception {
        final OWLClassR entity = new OWLClassR();
        entity.setUri(ENTITY_PK);
        when(cacheMock.contains(OWLClassR.class, ENTITY_PK, null)).thenReturn(true);
        when(cacheMock.get(OWLClassR.class, ENTITY_PK, null)).thenReturn(entity);
        final Types typesMock = mock(Types.class);
        final NamedResource individual = NamedResource.create(ENTITY_PK);
        final URI typeUri = URI.create(Vocabulary.C_OWLClassR);
        when(typesMock.getTypes(NamedResource.create(ENTITY_PK), null, false)).thenReturn(Collections.singleton(
                new AxiomImpl<>(individual, Assertion.createClassAssertion(false), new Value<>(typeUri))));
        when(connectionMock.types()).thenReturn(typesMock);

        final LoadingParameters<OWLClassS> loadingParameters = new LoadingParameters<>(OWLClassS.class, ENTITY_PK,
                aDescriptor);
        final OWLClassS result = mapper.loadEntity(loadingParameters);
        assertSame(entity, result);
        verify(cacheMock).get(OWLClassR.class, ENTITY_PK, null);
        verify(connectionMock, never()).find(any(AxiomDescriptor.class));
    }

    @Test
    public void loadEntityPutsItIntoSecondLevelCache() throws Exception {
        final Collection<Axiom<?>> axiomsForA = getAxiomsForEntityA();
        when(connectionMock.find(any(AxiomDescriptor.class))).thenReturn(axiomsForA);
        final OWLClassA result = mapper.loadEntity(loadingParameters);
        assertNotNull(result);
        verify(cacheMock).add(ENTITY_PK, result, null);
    }

    @Test
    public void loadEntityPutsIntoSecondLevelCacheThenEntityAndEntitiesItReferences() throws Exception {
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
        verify(cacheMock).add(identifier, result, null);
        verify(cacheMock).add(ENTITY_PK, result.getOwlClassA(), null);
    }

    private Collection<Axiom<?>> axiomsForD(URI identifier) {
        final NamedResource id = NamedResource.create(identifier);
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        axioms.add(new AxiomImpl<>(id, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.c_OwlClassD))));
        axioms.add(new AxiomImpl<>(id, Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false),
                new Value<Object>(NamedResource.create(ENTITY_PK))));
        return axioms;
    }

    @Test
    public void loadEntitySkipsCacheWhenNothingIsFound() throws Exception {
        when(connectionMock.find(any(AxiomDescriptor.class))).thenReturn(Collections.emptyList());
        final OWLClassA result = mapper.loadEntity(loadingParameters);
        assertNull(result);
        verify(cacheMock, never()).add(any(), any(), any());
    }
}
