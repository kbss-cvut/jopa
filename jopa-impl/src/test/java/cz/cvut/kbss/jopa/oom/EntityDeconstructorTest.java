package cz.cvut.kbss.jopa.oom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.util.HashSet;
import java.util.List;
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
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.utils.TestEnvironmentUtils;
import cz.cvut.kbss.ontodriver_new.MutationAxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Assertion.AssertionType;
import cz.cvut.kbss.ontodriver_new.model.Value;

public class EntityDeconstructorTest {

	private static OWLClassA entityA;
	private static URI strAttAIdentifier;

	@Mock
	private EntityType<OWLClassA> etAMock;
	@Mock
	private Attribute strAttAMock;
	@Mock
	private TypesSpecification typesMock;

	private EntityDeconstructor entityBreaker;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityA"));
		entityA.setStringAttribute("someStringAttribute");
		strAttAIdentifier = URI.create(OWLClassA.getStrAttField()
				.getAnnotation(OWLDataProperty.class).iri());
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		TestEnvironmentUtils.initOWLClassAMocks(etAMock, strAttAMock, typesMock);
		entityA.setTypes(null);
		this.entityBreaker = new EntityDeconstructor();
	}

	@Test
	public void testMapEntityWithDataProperty() {
		final Descriptor aDescriptor = new EntityDescriptor();
		final MutationAxiomDescriptor res = entityBreaker.mapEntityToAxioms(entityA.getUri(),
				entityA, etAMock, aDescriptor);
		assertNotNull(res);
		assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
		// Class assertion and the data property assertion
		assertEquals(2, res.getAssertions());
		assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
		assertTrue(res.getAssertions().contains(
				Assertion.createDataPropertyAssertion(strAttAIdentifier, false)));
	}

	@Test
	public void testMapEntityWithDataPropertyAndTypes() {
		final Descriptor aDescriptor = new EntityDescriptor();
		final Set<String> types = getTypes();
		entityA.setTypes(getTypes());
		final MutationAxiomDescriptor res = entityBreaker.mapEntityToAxioms(entityA.getUri(),
				entityA, etAMock, aDescriptor);
		assertNotNull(res);
		assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
		// Class assertion and the data property assertion
		assertEquals(2, res.getAssertions());
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

	private Set<String> getTypes() {
		final Set<String> types = new HashSet<>();
		types.add("http://krizik.felk.cvut.cz/ontologies/entityX");
		types.add("http://krizik.felk.cvut.cz/ontologies/entityY");
		return types;
	}
}
