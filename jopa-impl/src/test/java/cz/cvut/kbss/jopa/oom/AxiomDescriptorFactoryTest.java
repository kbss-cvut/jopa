package cz.cvut.kbss.jopa.oom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.Collections;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Attribute.PersistentAttributeType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.ontodriver_new.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

public class AxiomDescriptorFactoryTest {

	private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/entityA");

	private static URI stringAttrUri;

	@Mock
	private EntityType<OWLClassA> etAMock;
	@Mock
	private Attribute strAttMock;
	@Mock
	private TypesSpecification typesSpecMock;

	private AxiomDescriptorFactory factory;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		stringAttrUri = URI.create(OWLClassA.getStrAttField().getAnnotation(OWLDataProperty.class)
				.iri());
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(etAMock.getAttribute(OWLClassA.getStrAttField().getName())).thenReturn(strAttMock);
		when(etAMock.getTypes()).thenReturn(typesSpecMock);
		when(etAMock.getAttributes()).thenReturn(
				Collections.<Attribute<? super OWLClassA, ?>> singleton(strAttMock));
		when(strAttMock.getJavaField()).thenReturn(OWLClassA.getStrAttField());
		when(strAttMock.getIRI()).thenReturn(IRI.create(stringAttrUri.toString()));
		when(strAttMock.getPersistentAttributeType()).thenReturn(PersistentAttributeType.DATA);
		when(typesSpecMock.getJavaField()).thenReturn(OWLClassA.getTypesField());

		factory = new AxiomDescriptorFactory();
	}

	@Test
	public void testCreateForEntityLoadingSimple() throws Exception {
		final Descriptor desc = new EntityDescriptor();
		final AxiomDescriptor res = factory.createForEntityLoading(PK, desc, etAMock);
		// Types specification and the string attribute
		assertEquals(2, res.getAssertions().size());
		assertEquals(NamedResource.create(PK), res.getSubject());
		assertNull(res.getSubjectContext());
		assertTrue(res.getAssertions().contains(
				Assertion.createDataPropertyAssertion(stringAttrUri, false)));
	}

	@Test
	public void testCreateForFieldLoading() {
		fail("Not yet implemented");
	}

}
