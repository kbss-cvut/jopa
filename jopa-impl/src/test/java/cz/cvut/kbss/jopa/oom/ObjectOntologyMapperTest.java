package cz.cvut.kbss.jopa.oom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.utils.TestEnvironmentUtils;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.Connection;
import cz.cvut.kbss.ontodriver_new.MutationAxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

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
	private AxiomDescriptorFactory descriptorFactoryMock;

	@Mock
	private EntityConstructor entityConstructorMock;
	@Mock
	private EntityDeconstructor entityDeconstructorMock;

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
		when(descriptorFactoryMock.createForEntityLoading(ENTITY_PK, aDescriptor, etAMock))
				.thenReturn(axiomDescriptor);
		when(
				descriptorFactoryMock.createForFieldLoading(ENTITY_PK, OWLClassA.getTypesField(),
						aDescriptor, etAMock)).thenReturn(axiomDescriptor);
		when(metamodelMock.entity(OWLClassA.class)).thenReturn(etAMock);
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
		final Collection<Axiom> entityAAxioms = getAxiomsForEntityA();
		when(connectionMock.find(axiomDescriptor)).thenReturn(entityAAxioms);
		when(
				entityConstructorMock.reconstructEntity(ENTITY_PK, etAMock, aDescriptor,
						entityAAxioms)).thenReturn(entityA);
		final OWLClassA res = mapper.loadEntity(OWLClassA.class, ENTITY_PK, aDescriptor);

		assertNotNull(res);
		assertEquals(entityA.getUri(), res.getUri());
		assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
		verify(connectionMock).find(axiomDescriptor);
	}

	private Collection<Axiom> getAxiomsForEntityA() {
		final List<Axiom> res = new ArrayList<>();
		final Axiom clsAssertion = mock(Axiom.class);
		res.add(clsAssertion);
		final Axiom strAttAssertion = mock(Axiom.class);
		res.add(strAttAssertion);
		return res;
	}

	@Test
	public void testLoadEntityUnknown() throws Exception {
		when(connectionMock.find(axiomDescriptor)).thenReturn(Collections.<Axiom> emptyList());
		final OWLClassA res = mapper.loadEntity(OWLClassA.class, ENTITY_PK, aDescriptor);
		assertNull(res);
		verify(connectionMock).find(axiomDescriptor);
	}

	@Test(expected = StorageAccessException.class)
	public void testLoadEntityDriverException() throws Exception {
		when(connectionMock.find(axiomDescriptor)).thenThrow(new OntoDriverException());
		final OWLClassA res = mapper.loadEntity(OWLClassA.class, ENTITY_PK, aDescriptor);

		fail("This line should not have been reached.");
		assertNotNull(res);
	}

	@Test
	public void testLoadFieldValue() throws Exception {
		final Field typesField = OWLClassA.getTypesField();
		typesField.setAccessible(true);
		assertNull(typesField.get(entityA));
		final Collection<Axiom> axiomsForA = getAxiomsForEntityA();
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

		}).when(entityConstructorMock).setFieldValue(entityA, typesField, axiomsForA, etAMock);
		mapper.loadFieldValue(ENTITY_PK, entityA, typesField, aDescriptor);
		assertNotNull(typesField.get(entityA));
		assertEquals(aTypes, entityA.getTypes());
		verify(connectionMock).find(axiomDescriptor);
		verify(entityConstructorMock).setFieldValue(entityA, typesField, axiomsForA, etAMock);
	}

	@Test
	public void testLoadFieldValueEmpty() throws Exception {
		final Field typesField = OWLClassA.getTypesField();
		typesField.setAccessible(true);
		assertNull(typesField.get(entityA));
		when(connectionMock.find(axiomDescriptor)).thenReturn(Collections.<Axiom> emptyList());
		mapper.loadFieldValue(ENTITY_PK, entityA, typesField, aDescriptor);
		assertNull(typesField.get(entityA));
		verify(entityConstructorMock, never()).setFieldValue(any(OWLClassA.class), eq(typesField),
				any(Collection.class), any(EntityType.class));
	}

	@Test(expected = StorageAccessException.class)
	public void testLoadFieldValueStorageException() throws Exception {
		when(connectionMock.find(axiomDescriptor)).thenThrow(new OntoDriverException());
		final Field typesField = OWLClassA.getTypesField();
		typesField.setAccessible(true);
		assertNull(typesField.get(entityA));
		try {
			mapper.loadFieldValue(ENTITY_PK, entityA, typesField, aDescriptor);
			fail("This line should not have been reached.");
		} finally {
			verify(entityConstructorMock, never()).setFieldValue(any(OWLClassA.class),
					eq(typesField), any(Collection.class), any(EntityType.class));
		}
	}

	@Test
	public void testPersistEntity() throws Exception {
		final MutationAxiomDescriptor madMock = mock(MutationAxiomDescriptor.class);
		when(entityDeconstructorMock.mapEntityToAxioms(ENTITY_PK, entityA, etAMock, aDescriptor))
				.thenReturn(madMock);
		mapper.persistEntity(ENTITY_PK, entityA, aDescriptor);
		verify(connectionMock).persist(madMock);
	}

	@Test
	public void testSetIdentifier() throws Exception {
		final Identifier id = mock(Identifier.class);
		when(etAMock.getIdentifier()).thenReturn(id);
		when(id.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));
		final OWLClassA a = new OWLClassA();
		final URI pk = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tempUri");
		assertNull(a.getUri());
		mapper.setIdentifier(pk, a, etAMock);
		assertNotNull(a.getUri());
		assertEquals(pk, a.getUri());
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
		final Collection<Axiom> entityAAxioms = getAxiomsForEntityA();
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
}
