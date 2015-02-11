package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.oom.exceptions.UnpersistedChangeException;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.utils.TestEnvironmentUtils;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.Connection;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public class ObjectOntologyMapperTest {

    private static final URI ENTITY_PK = URI
            .create("http://krizik.felk.cvut.cz/ontologies/entityA");

    private static OWLClassA entityA;
    private static Descriptor aDescriptor;
    private static Set<String> aTypes;
    private static AxiomDescriptor axiomDescriptor;

    @Mock
    private UnitOfWorkImpl uowMock;

    @Mock
    private Connection connectionMock;

    @Mock
    private Metamodel metamodelMock;
    @Mock
    private CacheManager cacheMock;

    @Mock
    private EntityType<OWLClassA> etAMock;
    @Mock
    private Identifier idA;

    @Mock
    private AxiomDescriptorFactory descriptorFactoryMock;

    @Mock
    private EntityConstructor entityConstructorMock;
    @Mock
    private EntityDeconstructor entityDeconstructorMock;

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
        this.loadingParameters = new LoadingParameters<>(OWLClassA.class, ENTITY_PK, aDescriptor);
        when(descriptorFactoryMock.createForEntityLoading(loadingParameters, etAMock))
                .thenReturn(axiomDescriptor);
        when(
                descriptorFactoryMock.createForFieldLoading(ENTITY_PK, OWLClassA.getTypesField(),
                        aDescriptor, etAMock)).thenReturn(axiomDescriptor);
        when(metamodelMock.entity(OWLClassA.class)).thenReturn(etAMock);
        when(etAMock.getIdentifier()).thenReturn(idA);
        when(etAMock.getIRI()).thenReturn(IRI.create(OWLClassA.getClassIri()));
        when(idA.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));
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

    @Test
    public void testLoadEntity() throws Exception {
        final Collection<Axiom<?>> entityAAxioms = getAxiomsForEntityA();
        when(connectionMock.find(axiomDescriptor)).thenReturn(entityAAxioms);
        when(
                entityConstructorMock.reconstructEntity(ENTITY_PK, etAMock, aDescriptor,
                        entityAAxioms)).thenReturn(entityA);
        final OWLClassA res = mapper.loadEntity(loadingParameters);

        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getUri());
        assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
        verify(connectionMock).find(axiomDescriptor);
    }

    private Collection<Axiom<?>> getAxiomsForEntityA() {
        final List<Axiom<?>> res = new ArrayList<>();
        final Axiom<?> clsAssertion = mock(Axiom.class);
        res.add(clsAssertion);
        final Axiom<?> strAttAssertion = mock(Axiom.class);
        res.add(strAttAssertion);
        return res;
    }

    @Test
    public void testLoadEntityUnknown() throws Exception {
        when(connectionMock.find(axiomDescriptor)).thenReturn(Collections.<Axiom<?>>emptyList());
        final OWLClassA res = mapper.loadEntity(loadingParameters);
        assertNull(res);
        verify(connectionMock).find(axiomDescriptor);
    }

    @Test(expected = StorageAccessException.class)
    public void testLoadEntityDriverException() throws Exception {
        when(connectionMock.find(axiomDescriptor)).thenThrow(new OntoDriverException());
        final OWLClassA res = mapper.loadEntity(loadingParameters);

        fail("This line should not have been reached.");
        assertNotNull(res);
    }

    @Test
    public void testLoadFieldValue() throws Exception {
        final Field typesField = OWLClassA.getTypesField();
        typesField.setAccessible(true);
        assertNull(typesField.get(entityA));
        final Collection<Axiom<?>> axiomsForA = getAxiomsForEntityA();
        when(connectionMock.find(axiomDescriptor)).thenReturn(axiomsForA);
        doAnswer(new Answer<Void>() {

            @Override
            public Void answer(InvocationOnMock invocation) throws Throwable {
                final OWLClassA a = (OWLClassA) invocation.getArguments()[0];
                final Field types = (Field) invocation.getArguments()[1];
                types.setAccessible(true);
                types.set(a, aTypes);
                return null;
            }

        }).when(entityConstructorMock).setFieldValue(entityA, typesField, axiomsForA, etAMock,
                aDescriptor);
        mapper.loadFieldValue(entityA, typesField, aDescriptor);
        assertNotNull(typesField.get(entityA));
        assertEquals(aTypes, entityA.getTypes());
        verify(connectionMock).find(axiomDescriptor);
        verify(entityConstructorMock).setFieldValue(entityA, typesField, axiomsForA, etAMock,
                aDescriptor);
    }

    @Test
    public void testLoadFieldValueEmpty() throws Exception {
        final Field typesField = OWLClassA.getTypesField();
        typesField.setAccessible(true);
        assertNull(typesField.get(entityA));
        when(connectionMock.find(axiomDescriptor)).thenReturn(Collections.<Axiom<?>>emptyList());
        mapper.loadFieldValue(entityA, typesField, aDescriptor);
        assertNull(typesField.get(entityA));
        verify(entityConstructorMock, never()).setFieldValue(any(OWLClassA.class), eq(typesField),
                any(Collection.class), any(EntityType.class), eq(aDescriptor));
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
            verify(entityConstructorMock, never()).setFieldValue(any(OWLClassA.class),
                    eq(typesField), any(Collection.class), any(EntityType.class),
                    any(Descriptor.class));
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
        final Identifier id = mock(Identifier.class);
        when(etAMock.getIdentifier()).thenReturn(id);
        when(id.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));
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
    public void testGetEntityFromCacheOrOntologyLoadIt() throws Exception {
        when(cacheMock.contains(OWLClassA.class, ENTITY_PK, null)).thenReturn(Boolean.FALSE);
        final Collection<Axiom<?>> entityAAxioms = getAxiomsForEntityA();
        when(connectionMock.find(axiomDescriptor)).thenReturn(entityAAxioms);
        when(
                entityConstructorMock.reconstructEntity(ENTITY_PK, etAMock, aDescriptor,
                        entityAAxioms)).thenReturn(entityA);
        final OWLClassA res = mapper.getEntityFromCacheOrOntology(OWLClassA.class, ENTITY_PK,
                aDescriptor);
        assertSame(entityA, res);
        verify(entityConstructorMock).reconstructEntity(ENTITY_PK, etAMock, aDescriptor,
                entityAAxioms);
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
        when(connectionMock.find(any(AxiomDescriptor.class))).thenReturn(
                Collections.<Axiom<?>>emptyList());
        mapper.checkForUnpersistedChanges();
    }

    @Test
    public void removesEntityWithEmptyDescriptor() throws Exception {
        mapper.removeEntity(ENTITY_PK, OWLClassA.class, aDescriptor);
        verify(descriptorFactoryMock).createForEntityLoading(loadingParameters, etAMock);
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
}
