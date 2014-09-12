package cz.cvut.kbss.jopa.oom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.utils.Generators;
import cz.cvut.kbss.jopa.test.utils.TestEnvironmentUtils;
import cz.cvut.kbss.ontodriver_new.MutationAxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Assertion.AssertionType;
import cz.cvut.kbss.ontodriver_new.model.Value;

public class EntityDeconstructorTest {

	private static final URI CONTEXT = URI
			.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextOne");

	private static OWLClassA entityA;
	private static URI strAttAIdentifier;
	private static OWLClassB entityB;
	private static OWLClassD entityD;

	@Mock
	private EntityType<OWLClassA> etAMock;
	@Mock
	private Attribute strAttAMock;
	@Mock
	private TypesSpecification typesMock;
	@Mock
	private Identifier idA;

	@Mock
	private EntityType<OWLClassB> etBMock;
	@Mock
	private Attribute strAttBMock;
	@Mock
	private PropertiesSpecification propsMock;

	@Mock
	private EntityType<OWLClassD> etDMock;
	@Mock
	private Attribute clsAMock;

	@Mock
	private ObjectOntologyMapperImpl oomMock;

	private EntityDeconstructor entityBreaker;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityA"));
		entityA.setStringAttribute("someStringAttribute");
		strAttAIdentifier = URI.create(OWLClassA.getStrAttField()
				.getAnnotation(OWLDataProperty.class).iri());
		entityB = new OWLClassB();
		entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityB"));
		entityB.setStringAttribute("entityBStringAttribute");
		entityD = new OWLClassD();
		entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityD"));
		entityD.setOwlClassA(entityA);
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		TestEnvironmentUtils.initOWLClassAMocks(etAMock, strAttAMock, typesMock);
		when(etAMock.getIdentifier()).thenReturn(idA);
		when(idA.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));
		TestEnvironmentUtils.initOWLClassBMocks(etBMock, strAttBMock, propsMock);
		TestEnvironmentUtils.initOWLClassDMocks(etDMock, clsAMock);
		when(oomMock.getEntityType(OWLClassA.class)).thenReturn(etAMock);
		entityA.setTypes(null);
		entityB.setProperties(null);
		this.entityBreaker = new EntityDeconstructor(oomMock);
	}

	@Test
	public void testMapEntityWithDataProperty() {
		final Descriptor aDescriptor = new EntityDescriptor();
		final MutationAxiomDescriptor res = entityBreaker.mapEntityToAxioms(entityA.getUri(),
				entityA, etAMock, aDescriptor);
		assertNotNull(res);
		assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
		// Class assertion and the data property assertion
		assertEquals(2, res.getAssertions().size());
		assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
		assertTrue(res.getAssertions().contains(
				Assertion.createDataPropertyAssertion(strAttAIdentifier, false)));
	}

	@Test
	public void testMapEntityWithDataPropertyNullValue() {
		final OWLClassA entity = new OWLClassA();
		entity.setUri(entityA.getUri());
		final Descriptor aDescriptor = new EntityDescriptor();
		final MutationAxiomDescriptor res = entityBreaker.mapEntityToAxioms(entity.getUri(),
				entity, etAMock, aDescriptor);
		assertNotNull(res);
		assertEquals(entity.getUri(), res.getSubject().getIdentifier());
		// Only the class assertion
		assertEquals(1, res.getAssertions().size());
		assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
	}

	@Test
	public void testMapEntityWithDataPropertyAndTypes() {
		final Descriptor aDescriptor = new EntityDescriptor();
		final Set<String> types = createTypes();
		entityA.setTypes(types);
		final MutationAxiomDescriptor res = entityBreaker.mapEntityToAxioms(entityA.getUri(),
				entityA, etAMock, aDescriptor);
		assertNotNull(res);
		assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
		// Class assertion and the data property assertion
		assertEquals(2, res.getAssertions().size());
		assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
		assertTrue(res.getAssertions().contains(
				Assertion.createDataPropertyAssertion(strAttAIdentifier, false)));
		for (Assertion a : res.getAssertions()) {
			if (a.getType() == AssertionType.CLASS) {
				final List<Value<?>> clss = res.getAssertionValues(a);
				// The entity class + the declared types
				assertEquals(types.size() + 1, clss.size());
				for (Value<?> v : clss) {
					final String strVal = v.toString();
					assertTrue(OWLClassA.getClassIri().equals(strVal) || types.contains(strVal));
				}
			}
		}
	}

	private Set<String> createTypes() {
		final Set<String> types = new HashSet<>();
		types.add("http://krizik.felk.cvut.cz/ontologies/entityX");
		types.add("http://krizik.felk.cvut.cz/ontologies/entityY");
		return types;
	}

	@Test
	public void testMapEntityWithDataPropertyAndTypesPropertyInDifferentContext() throws Exception {
		final Descriptor aDescriptor = new EntityDescriptor();
		aDescriptor.addAttributeContext(OWLClassA.getStrAttField(), CONTEXT);
		final Set<String> types = createTypes();
		entityA.setTypes(types);
		final MutationAxiomDescriptor res = entityBreaker.mapEntityToAxioms(entityA.getUri(),
				entityA, etAMock, aDescriptor);
		assertNotNull(res);
		assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
		assertNull(res.getSubjectContext());
		assertEquals(CONTEXT, res.getAssertionContext(Assertion.createDataPropertyAssertion(
				strAttAIdentifier, false)));
		assertNull(res.getAssertionContext(Assertion.createClassAssertion(false)));
	}

	@Test
	public void testMapEntityWithObjectProperty() {
		final Descriptor dDescriptor = new EntityDescriptor();
		final MutationAxiomDescriptor res = entityBreaker.mapEntityToAxioms(entityD.getUri(),
				entityD, etDMock, dDescriptor);
		assertNotNull(res);
		assertEquals(entityD.getUri(), res.getSubject().getIdentifier());
		// Class assertion and the object property assertion
		assertEquals(2, res.getAssertions().size());
		for (Assertion a : res.getAssertions()) {
			final List<Value<?>> vals = res.getAssertionValues(a);
			if (a.getType() == AssertionType.CLASS) {
				assertEquals(1, vals.size());
				assertEquals(URI.create(OWLClassD.getClassIri()), vals.get(0).getValue());
			} else {
				assertTrue(AssertionType.OBJECT_PROPERTY == a.getType());
				assertEquals(1, vals.size());
				assertEquals(entityA.getUri(), vals.get(0).getValue());
			}
		}
	}

	@Test
	public void testMapEntityWithObjectPropertyNullValue() {
		final OWLClassD entity = new OWLClassD();
		entity.setUri(entityD.getUri());
		final Descriptor dDescriptor = new EntityDescriptor();
		final MutationAxiomDescriptor res = entityBreaker.mapEntityToAxioms(entity.getUri(),
				entity, etDMock, dDescriptor);
		assertNotNull(res);
		assertEquals(entity.getUri(), res.getSubject().getIdentifier());
		// Only the class assertion
		assertEquals(1, res.getAssertions().size());
		assertTrue(res.getAssertions().iterator().next().getType() == AssertionType.CLASS);
	}

	@Test
	public void testMapEntityWithObjectPropertyAndContext() {
		final Descriptor dDescriptor = new EntityDescriptor(CONTEXT);
		final MutationAxiomDescriptor res = entityBreaker.mapEntityToAxioms(entityD.getUri(),
				entityD, etDMock, dDescriptor);
		assertNotNull(res);
		assertEquals(entityD.getUri(), res.getSubject().getIdentifier());
		assertEquals(CONTEXT, res.getSubjectContext());
		for (Assertion ass : res.getAssertions()) {
			assertEquals(CONTEXT, res.getAssertionContext(ass));
		}
	}

	@Test
	public void testMapEntityWithProperties() {
		final Descriptor bDescriptor = new EntityDescriptor();
		final Map<String, Set<String>> props = Generators.createProperties();
		entityB.setProperties(props);
		final MutationAxiomDescriptor res = entityBreaker.mapEntityToAxioms(entityB.getUri(),
				entityB, etBMock, bDescriptor);
		assertEquals(entityB.getUri(), res.getSubject().getIdentifier());
		// Class assertion, data property assertion and the properties
		// assertions
		assertEquals(props.size() + 2, res.getAssertions().size());
		for (String p : props.keySet()) {
			assertTrue(res.getAssertions().contains(
					Assertion.createPropertyAssertion(URI.create(p), false)));
		}
	}

	@Test
	public void testMapEntityWithNullProperties() {
		final Descriptor bDescriptor = new EntityDescriptor();
		final MutationAxiomDescriptor res = entityBreaker.mapEntityToAxioms(entityB.getUri(),
				entityB, etBMock, bDescriptor);
		assertEquals(entityB.getUri(), res.getSubject().getIdentifier());
		// Class assertion, data property assertion
		assertEquals(2, res.getAssertions().size());
	}

	@Test
	public void testMapEntityWithPropertiesMultipleValuesPerProperty() {
		final Map<String, Set<String>> props = createProperties();
		entityB.setProperties(props);
		final Descriptor bDescriptor = new EntityDescriptor();
		final MutationAxiomDescriptor res = entityBreaker.mapEntityToAxioms(entityB.getUri(),
				entityB, etBMock, bDescriptor);
		assertEquals(entityB.getUri(), res.getSubject().getIdentifier());
		assertEquals(props.size() + 2, res.getAssertions().size());
		for (Entry<String, Set<String>> e : props.entrySet()) {
			final Set<String> expVals = e.getValue();
			final List<Value<?>> vals = res.getAssertionValues(Assertion.createPropertyAssertion(
					URI.create(e.getKey()), false));
			assertEquals(expVals.size(), vals.size());
			for (Value<?> val : vals) {
				assertTrue(expVals.contains(val.getValue()));
			}
		}
	}

	private Map<String, Set<String>> createProperties() {
		final Map<String, Set<String>> map = new HashMap<>();
		for (int i = 0; i < 5; i++) {
			final String key = "http://krizik.felk.cvut.cz/ontologies/jopa/someProperty" + i;
			final Set<String> vals = new HashSet<>();
			for (int j = 0; j < 5; j++) {
				vals.add("dataValue" + j);
			}
			map.put(key, vals);
		}
		return map;
	}
}
