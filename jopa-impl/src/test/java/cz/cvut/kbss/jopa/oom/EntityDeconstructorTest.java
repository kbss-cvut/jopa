package cz.cvut.kbss.jopa.oom;

import static org.junit.Assert.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
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
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassK;
import cz.cvut.kbss.jopa.test.utils.Generators;
import cz.cvut.kbss.jopa.test.utils.TestEnvironmentUtils;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Assertion.AssertionType;
import cz.cvut.kbss.ontodriver_new.model.Value;

public class EntityDeconstructorTest {

	private static final URI CONTEXT = URI
			.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextOne");

	private static OWLClassA entityA;
	private static URI strAttAIdentifier;
	private static OWLClassB entityB;
	private static OWLClassE entityE;
	private static OWLClassD entityD;
	private static URI owlclassAAttIdentifier;
	private static OWLClassK entityK;

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
	private EntityType<OWLClassE> etEMock;
	@Mock
	private Attribute strAttEMock;
	@Mock
	private Identifier idE;

	@Mock
	private EntityType<OWLClassD> etDMock;
	@Mock
	private Attribute clsAMock;

	@Mock
	private EntityType<OWLClassK> etKMock;
	@Mock
	private Attribute clsEMock;
	@Mock
	private Identifier idK;

	@Mock
	private ObjectOntologyMapperImpl oomMock;

	@Mock
	private CascadeResolver cascadeResolverMock;

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
		entityE = new OWLClassE();
		entityE.setStringAttribute("entityEStringAttribute");
		entityD = new OWLClassD();
		entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityD"));
		entityD.setOwlClassA(entityA);
		owlclassAAttIdentifier = URI.create(OWLClassD.getOwlClassAField()
				.getAnnotation(OWLObjectProperty.class).iri());
		entityK = new OWLClassK();
		entityK.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityD"));
		entityK.setOwlClassE(entityE);
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		TestEnvironmentUtils.initOWLClassAMocks(etAMock, strAttAMock, typesMock);
		when(etAMock.getIdentifier()).thenReturn(idA);
		when(idA.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));
		TestEnvironmentUtils.initOWLClassBMocks(etBMock, strAttBMock, propsMock);
		TestEnvironmentUtils.initOWLClassEMocks(etEMock, strAttEMock, idE);
		TestEnvironmentUtils.initOWLClassDMocks(etDMock, clsAMock);
		TestEnvironmentUtils.initOWLClassKMocks(etKMock, clsEMock, idK);
		when(oomMock.getEntityType(OWLClassA.class)).thenReturn(etAMock);
		entityA.setTypes(null);
		entityA.setStringAttribute("someStringAttribute");
		entityB.setProperties(null);
		entityE.setUri(null);
		this.entityBreaker = new EntityDeconstructor(oomMock);
		entityBreaker.setCascadeResolver(cascadeResolverMock);
	}

	@Test
	public void testMapEntityWithDataProperty() throws Exception {
		final Descriptor aDescriptor = new EntityDescriptor();
		final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityA.getUri(),
				entityA, etAMock, aDescriptor);
		final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
		assertNotNull(res);
		assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
		// Class assertion and the data property assertion
		assertEquals(2, res.getAssertions().size());
		assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
		assertTrue(res.getAssertions().contains(
				Assertion.createDataPropertyAssertion(strAttAIdentifier, false)));
	}

	@Test
	public void testMapEntityWithDataPropertyNullValue() throws Exception {
		final OWLClassA entity = new OWLClassA();
		entity.setUri(entityA.getUri());
		final Descriptor aDescriptor = new EntityDescriptor();
		final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entity.getUri(), entity,
				etAMock, aDescriptor);
		final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
		assertNotNull(res);
		assertEquals(entity.getUri(), res.getSubject().getIdentifier());
		assertEquals(2, res.getAssertions().size());
		assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
		final List<Value<?>> v = res.getAssertionValues(Assertion.createDataPropertyAssertion(
				strAttAIdentifier, false));
		assertEquals(1, v.size());
		assertEquals(Value.nullValue(), v.get(0));
	}

	@Test
	public void testMapEntityWithDataPropertyAndTypes() throws Exception {
		final Descriptor aDescriptor = new EntityDescriptor();
		final Set<String> types = createTypes();
		entityA.setTypes(types);
		final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityA.getUri(),
				entityA, etAMock, aDescriptor);
		final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
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
				assertEquals(1, clss.size());
				assertEquals(OWLClassA.getClassIri(), clss.get(0).stringValue());
			}
		}
		final Set<URI> typesRes = OOMTestUtils.getTypes(builder);
		assertEquals(types.size(), typesRes.size());
		for (URI u : typesRes) {
			assertTrue(types.contains(u.toString()));
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
		final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityA.getUri(),
				entityA, etAMock, aDescriptor);
		final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
		assertNotNull(res);
		assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
		assertNull(res.getSubjectContext());
		assertEquals(CONTEXT, res.getAssertionContext(Assertion.createDataPropertyAssertion(
				strAttAIdentifier, false)));
		assertNull(res.getAssertionContext(Assertion.createClassAssertion(false)));
	}

	@Test
	public void testMapEntityWithObjectProperty() throws Exception {
		final Descriptor dDescriptor = new EntityDescriptor();
		final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityD.getUri(),
				entityD, etDMock, dDescriptor);
		final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
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
				assertEquals(NamedResource.create(entityA.getUri()), vals.get(0).getValue());
			}
		}
	}

	@Test
	public void testMapEntityWithObjectPropertyNullValue() throws Exception {
		final OWLClassD entity = new OWLClassD();
		entity.setUri(entityD.getUri());
		final Descriptor dDescriptor = new EntityDescriptor();
		final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entity.getUri(), entity,
				etDMock, dDescriptor);
		final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
		assertNotNull(res);
		assertEquals(entity.getUri(), res.getSubject().getIdentifier());
		// Only the class assertion
		assertEquals(2, res.getAssertions().size());
		assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
		final List<Value<?>> v = res.getAssertionValues(Assertion.createObjectPropertyAssertion(
				owlclassAAttIdentifier, false));
		assertEquals(1, v.size());
		assertEquals(Value.nullValue(), v.get(0));
	}

	@Test
	public void testMapEntityWithObjectPropertyAndContext() throws Exception {
		final Descriptor dDescriptor = new EntityDescriptor(CONTEXT);
		final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityD.getUri(),
				entityD, etDMock, dDescriptor);
		final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
		assertNotNull(res);
		assertEquals(entityD.getUri(), res.getSubject().getIdentifier());
		assertEquals(CONTEXT, res.getSubjectContext());
		for (Assertion ass : res.getAssertions()) {
			assertEquals(CONTEXT, res.getAssertionContext(ass));
		}
	}

	@Test
	public void testMapEntityWithProperties() throws Exception {
		final Descriptor bDescriptor = new EntityDescriptor();
		final Map<String, Set<String>> props = Generators.createProperties();
		entityB.setProperties(props);
		final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityB.getUri(),
				entityB, etBMock, bDescriptor);
		final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
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
	public void testMapEntityWithNullProperties() throws Exception {
		final Descriptor bDescriptor = new EntityDescriptor();
		final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityB.getUri(),
				entityB, etBMock, bDescriptor);
		final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
		assertEquals(entityB.getUri(), res.getSubject().getIdentifier());
		// Class assertion, data property assertion
		assertEquals(2, res.getAssertions().size());
	}

	@Test
	public void testMapEntityWithPropertiesMultipleValuesPerProperty() throws Exception {
		final Map<String, Set<String>> props = createProperties();
		entityB.setProperties(props);
		final Descriptor bDescriptor = new EntityDescriptor();
		final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityB.getUri(),
				entityB, etBMock, bDescriptor);
		final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
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

	@Test
	public void testMapEntityWithObjectPropertyWithGeneratedIdentifier() throws Exception {
		final URI eUri = URI.create("http://eUri");
		when(oomMock.generateIdentifier(etEMock)).thenReturn(eUri);
		when(oomMock.getEntityType(OWLClassE.class)).thenReturn(etEMock);
		final Descriptor eDescriptor = new EntityDescriptor(CONTEXT);
		final Descriptor kDescriptor = new EntityDescriptor();
		kDescriptor.addAttributeDescriptor(OWLClassK.getOwlClassEField(), eDescriptor);
		assertNull(entityE.getUri());
		final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityK.getUri(),
				entityK, etKMock, kDescriptor);
		final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
		assertNotNull(res);
		verify(oomMock).generateIdentifier(etEMock);
		assertEquals(eUri, entityE.getUri());
	}

	@Test
	public void mapsEntityDataPropertyFieldToAxiomDescriptor() throws Exception {
		final Descriptor aDescriptor = new EntityDescriptor();
		when(etAMock.getFieldSpecification(OWLClassA.getStrAttField().getName())).thenReturn(
				strAttAMock);
		final AxiomValueGatherer builder = entityBreaker.mapFieldToAxioms(entityA.getUri(), entityA,
				OWLClassA.getStrAttField(), etAMock, aDescriptor);
		final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
		assertEquals(1, res.getAssertions().size());
		assertTrue(res.getAssertions().contains(
				Assertion.createDataPropertyAssertion(strAttAIdentifier, strAttAMock.isInferred())));
		final List<Value<?>> val = res.getAssertionValues(res.getAssertions().iterator().next());
		assertEquals(1, val.size());
		assertEquals(entityA.getStringAttribute(), val.get(0).getValue());
	}

	@Test
	public void mapsEntityDataPropertyWithNullValueToAxiomDescriptor() throws Exception {
		final Descriptor aDescriptor = new EntityDescriptor();
		entityA.setStringAttribute(null);
		when(etAMock.getFieldSpecification(OWLClassA.getStrAttField().getName())).thenReturn(
				strAttAMock);
		final AxiomValueGatherer builder = entityBreaker.mapFieldToAxioms(entityA.getUri(), entityA,
				OWLClassA.getStrAttField(), etAMock, aDescriptor);
		final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
		assertEquals(1, res.getAssertions().size());
		assertTrue(res.getAssertions().contains(
				Assertion.createDataPropertyAssertion(strAttAIdentifier, strAttAMock.isInferred())));
		final List<Value<?>> val = res.getAssertionValues(res.getAssertions().iterator().next());
		assertEquals(1, val.size());
		assertEquals(Value.nullValue(), val.get(0));
		assertNull(val.get(0).getValue());
	}

	@Test
	public void mapsEntityObjectPropertyValueInContextToAxiomDescriptor() throws Exception {
		final Descriptor dDescriptor = new EntityDescriptor();
		dDescriptor.addAttributeContext(OWLClassD.getOwlClassAField(), CONTEXT);
		when(etDMock.getFieldSpecification(OWLClassD.getOwlClassAField().getName())).thenReturn(
				clsAMock);
		final AxiomValueGatherer builder = entityBreaker.mapFieldToAxioms(entityD.getUri(), entityD,
				OWLClassD.getOwlClassAField(), etDMock, dDescriptor);
		final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
		assertEquals(1, res.getAssertions().size());
		assertTrue(res.getAssertions().contains(
				Assertion.createObjectPropertyAssertion(owlclassAAttIdentifier,
						strAttAMock.isInferred())));
		final Assertion ass = res.getAssertions().iterator().next();
		final List<Value<?>> val = res.getAssertionValues(ass);
		assertEquals(1, val.size());
		assertEquals(CONTEXT, res.getAssertionContext(ass));
		assertEquals(NamedResource.create(entityA.getUri()), val.get(0).getValue());
	}

	private static AxiomValueDescriptor getAxiomValueDescriptor(AxiomValueGatherer builder)
			throws Exception {
		return OOMTestUtils.getAxiomValueDescriptor(builder);
	}
}
