package cz.cvut.kbss.jopa.oom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.Collections;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Attribute.PersistentAttributeType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.utils.CommonConstants;
import cz.cvut.kbss.ontodriver_new.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

public class AxiomDescriptorFactoryTest {

	private static final URI CONTEXT = URI
			.create("http://krizik.felk.cvut.cz/ontologies/contextOne");
	private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/entityX");

	private static URI stringAttAUri;
	private static URI stringAttBUri;
	private static URI owlclassAAttUri;

	@Mock
	private EntityType<OWLClassA> etAMock;
	@Mock
	private Attribute strAttAMock;
	@Mock
	private TypesSpecification typesSpecMock;

	@Mock
	private EntityType<OWLClassB> etBMock;
	@Mock
	private Attribute strAttBMock;
	@Mock
	private PropertiesSpecification propsSpecMock;

	@Mock
	private EntityType<OWLClassD> etDMock;
	@Mock
	private Attribute owlclassAAttMock;

	private AxiomDescriptorFactory factory;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		stringAttAUri = URI.create(OWLClassA.getStrAttField().getAnnotation(OWLDataProperty.class)
				.iri());
		stringAttBUri = URI.create(OWLClassB.getStrAttField().getAnnotation(OWLDataProperty.class)
				.iri());
		owlclassAAttUri = URI.create(OWLClassD.getOwlClassAField()
				.getAnnotation(OWLObjectProperty.class).iri());
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		TestEnvironmentUtils.initOWLClassAMocks(etAMock, strAttAMock, typesSpecMock);
		TestEnvironmentUtils.initOWLClassBMocks(etBMock, strAttBMock, propsSpecMock);
		when(etDMock.getAttributes()).thenReturn(
				Collections.<Attribute<? super OWLClassD, ?>> singleton(owlclassAAttMock));
		when(etDMock.getAttribute(OWLClassD.getOwlClassAField().getName())).thenReturn(
				owlclassAAttMock);
		when(owlclassAAttMock.getJavaField()).thenReturn(OWLClassD.getOwlClassAField());
		when(owlclassAAttMock.getIRI()).thenReturn(IRI.create(owlclassAAttUri.toString()));
		when(owlclassAAttMock.getPersistentAttributeType()).thenReturn(
				PersistentAttributeType.OBJECT);
		when(owlclassAAttMock.getFetchType()).thenReturn(FetchType.EAGER);

		factory = new AxiomDescriptorFactory();
	}

	@Test
	public void testCreateForEntityLoadingWithTypes() throws Exception {
		final Descriptor desc = new EntityDescriptor();
		final AxiomDescriptor res = factory.createForEntityLoading(PK, desc, etAMock);
		// Types specification and the string attribute
		assertEquals(2, res.getAssertions().size());
		assertEquals(NamedResource.create(PK), res.getSubject());
		assertNull(res.getSubjectContext());
		assertTrue(res.getAssertions().contains(
				Assertion.createDataPropertyAssertion(stringAttAUri, false)));
		assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
	}

	@Test
	public void testCreateForEntityLoadingWithTypesInContext() throws Exception {
		final Descriptor desc = new EntityDescriptor();
		desc.addAttributeContext(OWLClassA.getTypesField(), CONTEXT);
		final AxiomDescriptor res = factory.createForEntityLoading(PK, desc, etAMock);
		// Types specification and the string attribute
		assertEquals(2, res.getAssertions().size());
		assertEquals(NamedResource.create(PK), res.getSubject());
		assertNull(res.getSubjectContext());
		assertTrue(res.getAssertions().contains(
				Assertion.createDataPropertyAssertion(stringAttAUri, false)));
		assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
		assertEquals(CONTEXT, res.getAssertionContext(Assertion.createClassAssertion(false)));
	}

	@Test
	public void testCreateForEntityLoadingWithPropertiesAndContext() throws Exception {
		final Descriptor desc = new EntityDescriptor(CONTEXT);
		final AxiomDescriptor res = factory.createForEntityLoading(PK, desc, etBMock);
		// Types specification and the string attribute
		assertEquals(2, res.getAssertions().size());
		assertEquals(NamedResource.create(PK), res.getSubject());
		assertEquals(CONTEXT, res.getSubjectContext());
		assertTrue(res.getAssertions().contains(
				Assertion.createDataPropertyAssertion(stringAttBUri, false)));
	}

	@Test
	public void testCreateForEntityLoadingWithObjectPropertyInContext() throws Exception {
		final Descriptor desc = new EntityDescriptor();
		desc.addAttributeContext(OWLClassD.getOwlClassAField(), CONTEXT);
		final AxiomDescriptor res = factory.createForEntityLoading(PK, desc, etDMock);
		assertEquals(1, res.getAssertions().size());
		assertEquals(NamedResource.create(PK), res.getSubject());
		assertNull(res.getSubjectContext());
		final Assertion ass = res.getAssertions().iterator().next();
		assertEquals(CONTEXT, res.getAssertionContext(ass));
		assertEquals(owlclassAAttUri, ass.getIdentifier());
	}

	@Test
	public void testCreateForEntityLoadingWithAnnotationProperty() throws Exception {
		final Descriptor desc = new EntityDescriptor();
		// Artificially change the attribute type to annotation
		when(owlclassAAttMock.getPersistentAttributeType()).thenReturn(
				PersistentAttributeType.ANNOTATION);
		final AxiomDescriptor res = factory.createForEntityLoading(PK, desc, etDMock);
		assertEquals(1, res.getAssertions().size());
		assertEquals(NamedResource.create(PK), res.getSubject());
		assertNull(res.getSubjectContext());
		assertTrue(res.getAssertions().contains(
				Assertion.createAnnotationPropertyAssertion(owlclassAAttUri, false)));
	}

	@Test
	public void createForEntityLoadingWithLazilyLoadedAttribute() throws Exception {
		final Descriptor desc = new EntityDescriptor();
		when(strAttAMock.getFetchType()).thenReturn(FetchType.LAZY);
		final AxiomDescriptor res = factory.createForEntityLoading(PK, desc, etAMock);
		// Types specification and the string attribute
		assertEquals(1, res.getAssertions().size());
		assertEquals(NamedResource.create(PK), res.getSubject());
		assertNull(res.getSubjectContext());
		assertFalse(res.getAssertions().contains(
				Assertion.createDataPropertyAssertion(stringAttAUri, false)));
		assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
	}

	@Test
	public void testCreateForFieldLoadingDataProperty() throws Exception {
		final Descriptor desc = new EntityDescriptor();
		when(strAttAMock.getFetchType()).thenReturn(FetchType.LAZY);
		final AxiomDescriptor res = factory.createForFieldLoading(PK, OWLClassA.getStrAttField(),
				desc, etAMock);
		assertNotNull(res);
		assertEquals(1, res.getAssertions().size());
		assertTrue(res.getAssertions().contains(
				Assertion.createDataPropertyAssertion(stringAttAUri, false)));
	}

	@Test
	public void testCreateForFieldLoadingObjectPropertyInContext() throws Exception {
		final Descriptor desc = new EntityDescriptor();
		desc.addAttributeDescriptor(OWLClassD.getOwlClassAField(), new EntityDescriptor(CONTEXT));
		final AxiomDescriptor res = factory.createForFieldLoading(PK,
				OWLClassD.getOwlClassAField(), desc, etDMock);
		assertEquals(1, res.getAssertions().size());
		final Assertion as = res.getAssertions().iterator().next();
		assertEquals(Assertion.createObjectPropertyAssertion(owlclassAAttUri, false), as);
		assertEquals(CONTEXT, res.getAssertionContext(as));
	}

	@Test
	public void testCreateForFieldLoadingTypes() throws Exception {
		final Descriptor desc = new EntityDescriptor(CONTEXT);
		final AxiomDescriptor res = factory.createForFieldLoading(PK, OWLClassA.getTypesField(),
				desc, etAMock);
		assertEquals(1, res.getAssertions().size());
		final Assertion as = res.getAssertions().iterator().next();
		assertEquals(Assertion.createClassAssertion(typesSpecMock.isInferred()), as);
		assertEquals(CONTEXT, res.getAssertionContext(as));
	}

	@Test
	public void testCreateForFieldLoadingProperties() throws Exception {
		final Descriptor desc = new EntityDescriptor();
		final AxiomDescriptor res = factory.createForFieldLoading(PK,
				OWLClassB.getPropertiesField(), desc, etBMock);
		assertEquals(1, res.getAssertions().size());
		final Assertion as = res.getAssertions().iterator().next();
		assertEquals(
				Assertion.createPropertyAssertion(CommonConstants.PROPERTIES_URI,
						propsSpecMock.isInferred()), as);
	}
}
