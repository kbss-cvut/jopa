package cz.cvut.kbss.jopa.oom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
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
import cz.cvut.kbss.jopa.test.utils.TestEnvironmentUtils;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

public class EntityConstructorTest {

	private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityX");
	private static final String STRING_ATT = "StringAttributeValue";
	private static final Set<String> TYPES = initTypes();

	@Mock
	private ObjectOntologyMapperImpl mapperMock;

	@Mock
	private EntityType<OWLClassA> etAMock;
	@Mock
	private Attribute strAttAMock;
	@Mock
	private TypesSpecification typesSpecMock;
	@Mock
	private Identifier idAMock;

	@Mock
	private EntityType<OWLClassB> etBMock;
	@Mock
	private Attribute strAttBMock;
	@Mock
	private PropertiesSpecification propsSpecMock;
	@Mock
	private Identifier idBMock;

	@Mock
	private EntityType<OWLClassD> etDMock;
	@Mock
	private Attribute clsAAttMock;
	@Mock
	private Identifier idDMock;

	private Descriptor descriptor;

	private EntityConstructor constructor;

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		TestEnvironmentUtils.initOWLClassAMocks(etAMock, strAttAMock, typesSpecMock);
		when(etAMock.getIdentifier()).thenReturn(idAMock);
		when(idAMock.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));
		TestEnvironmentUtils.initOWLClassBMocks(etBMock, strAttBMock, propsSpecMock);
		when(etBMock.getIdentifier()).thenReturn(idBMock);
		when(idBMock.getJavaField()).thenReturn(OWLClassB.class.getDeclaredField("uri"));
		TestEnvironmentUtils.initOWLClassDMocks(etDMock, clsAAttMock);
		when(etDMock.getIdentifier()).thenReturn(idDMock);
		when(idDMock.getJavaField()).thenReturn(OWLClassD.class.getDeclaredField("uri"));
		this.descriptor = new EntityDescriptor();
		this.constructor = new EntityConstructor(mapperMock);
	}

	@Test
	public void testReconstructEntityWithTypesAndDataProperty() throws Exception {
		final Set<Axiom> axioms = new HashSet<>();
		axioms.add(getClassAssertionAxiomForType(OWLClassA.getClassIri()));
		axioms.add(getStringAttAssertionAxiom(OWLClassA.getStrAttField()));
		axioms.addAll(getTypesAxiomsForOwlClassA());
		final OWLClassA res = constructor.reconstructEntity(PK, etAMock, descriptor, axioms);
		assertNotNull(res);
		assertEquals(PK, res.getUri());
		assertEquals(STRING_ATT, res.getStringAttribute());
		assertEquals(TYPES, res.getTypes());
		verify(mapperMock).registerInstance(PK, res, descriptor.getContext());
	}

	private Axiom getClassAssertionAxiomForType(String type) {
		final Axiom ax = mock(Axiom.class);
		when(ax.getSubject()).thenReturn(NamedResource.create(PK));
		when(ax.getAssertion()).thenReturn(Assertion.createClassAssertion(false));
		when(ax.getValue()).thenReturn(new Value<Object>(URI.create(type)));
		return ax;
	}

	private Axiom getStringAttAssertionAxiom(Field attField) throws Exception {
		final Axiom ax = mock(Axiom.class);
		when(ax.getSubject()).thenReturn(NamedResource.create(PK));
		final String assertionIri = attField.getAnnotation(OWLDataProperty.class).iri();
		when(ax.getAssertion()).thenReturn(
				Assertion.createDataPropertyAssertion(URI.create(assertionIri), false));
		when(ax.getValue()).thenReturn(new Value<Object>(STRING_ATT));
		return ax;
	}

	private Set<Axiom> getTypesAxiomsForOwlClassA() throws Exception {
		final Set<Axiom> axs = new HashSet<>();
		for (String type : TYPES) {
			final Axiom ax = mock(Axiom.class);
			when(ax.getSubject()).thenReturn(NamedResource.create(PK));
			when(ax.getAssertion()).thenReturn(Assertion.createClassAssertion(false));
			when(ax.getValue()).thenReturn(new Value<Object>(URI.create(type)));
			axs.add(ax);
		}
		return axs;
	}

	@Test
	public void testReconstructEntityWithDataPropertyEmptyTypes() throws Exception {
		final Set<Axiom> axioms = new HashSet<>();
		axioms.add(getClassAssertionAxiomForType(OWLClassA.getClassIri()));
		axioms.add(getStringAttAssertionAxiom(OWLClassA.getStrAttField()));
		final OWLClassA res = constructor.reconstructEntity(PK, etAMock, descriptor, axioms);
		assertNotNull(res);
		assertEquals(PK, res.getUri());
		assertEquals(STRING_ATT, res.getStringAttribute());
		assertNull(res.getTypes());
		verify(mapperMock).registerInstance(PK, res, descriptor.getContext());
	}

	@Test
	public void testReconstructEntityWithDataPropertyAndProperties() throws Exception {
		final Set<Axiom> axioms = new HashSet<>();
		axioms.add(getClassAssertionAxiomForType(OWLClassB.getClassIri()));
		axioms.add(getStringAttAssertionAxiom(OWLClassB.getStrAttField()));
		final Collection<Axiom> properties = getProperties();
		axioms.addAll(properties);
		final OWLClassB res = constructor.reconstructEntity(PK, etBMock, descriptor, axioms);
		assertNotNull(res);
		assertEquals(PK, res.getUri());
		assertEquals(STRING_ATT, res.getStringAttribute());
		assertNotNull(res.getProperties());
		assertEquals(properties.size(), res.getProperties().size());
		for (Axiom a : properties) {
			final String key = a.getAssertion().getIdentifier().toString();
			assertTrue(res.getProperties().containsKey(key));
			assertEquals(a.getValue().getValue().toString(), res.getProperties().get(key));
		}
		verify(mapperMock).registerInstance(PK, res, descriptor.getContext());
	}

	private Collection<Axiom> getProperties() {
		final Set<Axiom> props = new HashSet<>();
		final Axiom axOne = mock(Axiom.class);
		when(axOne.getSubject()).thenReturn(NamedResource.create(PK));
		when(axOne.getAssertion()).thenReturn(
				Assertion.createDataPropertyAssertion(URI.create("http://someDataPropertyOne"),
						false));
		when(axOne.getValue()).thenReturn(new Value<Object>("SomePropertyValue"));
		props.add(axOne);

		final Axiom axTwo = mock(Axiom.class);
		when(axTwo.getSubject()).thenReturn(NamedResource.create(PK));
		when(axTwo.getAssertion()).thenReturn(
				Assertion.createAnnotationPropertyAssertion(
						URI.create("http://someAnnotationPropertyOne"), false));
		when(axTwo.getValue()).thenReturn(new Value<Object>("annotationValue"));
		props.add(axTwo);

		final Axiom axThree = mock(Axiom.class);
		when(axThree.getSubject()).thenReturn(NamedResource.create(PK));
		when(axThree.getAssertion()).thenReturn(
				Assertion.createObjectPropertyAssertion(URI.create("http://someObjectPropertyOne"),
						false));
		when(axThree.getValue()).thenReturn(
				new Value<Object>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/otherEntity")));
		props.add(axThree);
		return props;
	}

	@Test
	public void testReconstructEntityWithDataPropertiesAndEmptyProperties() throws Exception {
		final Set<Axiom> axioms = new HashSet<>();
		axioms.add(getClassAssertionAxiomForType(OWLClassB.getClassIri()));
		axioms.add(getStringAttAssertionAxiom(OWLClassB.getStrAttField()));
		final OWLClassB res = constructor.reconstructEntity(PK, etBMock, descriptor, axioms);
		assertNotNull(res);
		assertEquals(PK, res.getUri());
		assertEquals(STRING_ATT, res.getStringAttribute());
		assertNull(res.getProperties());
		verify(mapperMock).registerInstance(PK, res, descriptor.getContext());
	}

	@Test
	public void testReconstructEntityWithObjectProperty() throws Exception {
		final Set<Axiom> axiomsD = getAxiomsForD();
		final Descriptor fieldDesc = mock(Descriptor.class);
		descriptor.addAttributeDescriptor(OWLClassD.getOwlClassAField(), fieldDesc);
		final OWLClassA entityA = new OWLClassA();
		entityA.setUri(PK);
		entityA.setStringAttribute(STRING_ATT);
		when(clsAAttMock.getFetchType()).thenReturn(FetchType.EAGER);
		when(mapperMock.getEntityFromCacheOrOntology(OWLClassA.class, PK, fieldDesc)).thenReturn(
				entityA);
		final OWLClassD res = constructor.reconstructEntity(PK, etDMock, descriptor, axiomsD);
		assertNotNull(res);
		assertEquals(PK, res.getUri());
		verify(mapperMock).getEntityFromCacheOrOntology(OWLClassA.class, PK, fieldDesc);
		assertNotNull(res.getOwlClassA());
		// Yes, we're using the same PK for both entities
		assertEquals(PK, res.getOwlClassA().getUri());
		assertEquals(STRING_ATT, res.getOwlClassA().getStringAttribute());
		verify(mapperMock).getEntityFromCacheOrOntology(OWLClassA.class, PK, fieldDesc);
		verify(mapperMock).registerInstance(PK, res, descriptor.getContext());
	}

	private Set<Axiom> getAxiomsForD() throws Exception {
		final Set<Axiom> axioms = new HashSet<>();
		axioms.add(getClassAssertionAxiomForType(OWLClassD.getClassIri()));
		final Axiom opAssertion = mock(Axiom.class);
		when(opAssertion.getSubject()).thenReturn(NamedResource.create(PK));
		final URI assertionUri = URI.create(OWLClassD.getOwlClassAField()
				.getAnnotation(OWLObjectProperty.class).iri());
		when(opAssertion.getAssertion()).thenReturn(
				Assertion.createObjectPropertyAssertion(assertionUri, false));
		when(opAssertion.getValue()).thenReturn(new Value<Object>(PK));
		axioms.add(opAssertion);
		return axioms;
	}

	@Test
	public void testSetFieldValue() {
		fail("Not yet implemented");
	}

	private static Set<String> initTypes() {
		final Set<String> set = new HashSet<>(8);
		set.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassU");
		set.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassV");
		return set;
	}
}
