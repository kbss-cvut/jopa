package cz.cvut.kbss.jopa.oom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.AxiomImpl;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

public class ReferencedListPropertyStrategyTest extends ListPropertyStrategyTestBase {

	private static List<OWLClassA> list;

	private ReferencedListPropertyStrategy<OWLClassC> strategy;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		list = generateList();
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		super.setUp();
		this.strategy = new ReferencedListPropertyStrategy<>(etC, refList, descriptor, mapperMock);
		strategy.setCascadeResolver(cascadeResolverMock);
	}

	@Test
	public void buildsInstanceFieldFromAxiomsIncludingNodes() throws Exception {
		final OWLClassC c = new OWLClassC(PK);
		final List<Axiom<?>> axioms = initRefListAxioms(true);
		when(mapperMock.loadReferencedList(any(ReferencedListDescriptor.class))).thenReturn(axioms);
		strategy.addValueFromAxiom(axioms.iterator().next());
		assertNull(c.getReferencedList());
		strategy.buildInstanceFieldValue(c);

		assertNotNull(c.getReferencedList());
		assertEquals(list, c.getReferencedList());
		final ArgumentCaptor<ReferencedListDescriptor> captor = ArgumentCaptor
				.forClass(ReferencedListDescriptor.class);
		verify(mapperMock).loadReferencedList(captor.capture());
		final ReferencedListDescriptor listDescriptor = captor.getValue();
		assertEquals(PK, listDescriptor.getListOwner().getIdentifier());
		assertEquals(refList.getIRI().toURI(), listDescriptor.getListProperty().getIdentifier());
		assertEquals(refList.getOWLObjectPropertyHasNextIRI().toURI(), listDescriptor.getNextNode()
				.getIdentifier());
		assertEquals(refList.getOWLPropertyHasContentsIRI().toURI(), listDescriptor
				.getNodeContent().getIdentifier());
	}

	private List<Axiom<?>> initRefListAxioms(boolean includeNodes) throws Exception {
		final List<Axiom<?>> axioms = new ArrayList<>();
		NamedResource previous = NamedResource.create(PK);
		int i = 0;
		for (OWLClassA a : list) {
			final URI nodeUri = URI
					.create("http://krizik.felk.cvut.cz/ontologies/jopa/refListNode_" + i);
			if (includeNodes) {
				final Axiom<URI> node;
				if (i == 0) {
					node = new AxiomImpl<>(previous, Assertion.createObjectPropertyAssertion(
							URI.create(OWLClassC.getRefListField()
									.getAnnotation(OWLObjectProperty.class).iri()),
							refList.isInferred()), new Value<>(nodeUri));
				} else {
					node = new AxiomImpl<>(
							previous,
							Assertion.createObjectPropertyAssertion(refList
									.getOWLObjectPropertyHasNextIRI().toURI(), refList.isInferred()),
							new Value<>(nodeUri));
				}
				axioms.add(node);
			}
			final Axiom<URI> content = new AxiomImpl<>(NamedResource.create(nodeUri),
					Assertion.createObjectPropertyAssertion(refList.getOWLPropertyHasContentsIRI()
							.toURI(), refList.isInferred()), new Value<>(a.getUri()));
			when(mapperMock.getEntityFromCacheOrOntology(OWLClassA.class, a.getUri(), descriptor))
					.thenReturn(a);
			axioms.add(content);
			previous = NamedResource.create(nodeUri);
			i++;
		}
		return axioms;
	}

	/**
	 * No node axioms, only content axioms.
	 * 
	 * @throws Exception
	 */
	@Test
	public void buildsInstanceFieldFromAxiomsWithoutNodes() throws Exception {
		final OWLClassC c = new OWLClassC(PK);
		final List<Axiom<?>> axioms = initRefListAxioms(false);
		when(mapperMock.loadReferencedList(any(ReferencedListDescriptor.class))).thenReturn(axioms);
		strategy.addValueFromAxiom(axioms.iterator().next());
		assertNull(c.getReferencedList());
		strategy.buildInstanceFieldValue(c);

		assertNotNull(c.getReferencedList());
		assertEquals(list, c.getReferencedList());
	}

	@Test
	public void extractsValuesIntoAxiomsForSave() throws Exception {
		final OWLClassC c = new OWLClassC(PK);
		c.setReferencedList(list);
		final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK),
				descriptor.getContext());
		strategy.buildAxiomValuesFromInstance(c, builder);

		final List<ReferencedListValueDescriptor> descriptors = OOMTestUtils
				.getReferencedListValueDescriptors(builder);
		assertEquals(1, descriptors.size());
		final ReferencedListValueDescriptor res = descriptors.get(0);
		assertEquals(res.getListOwner(), NamedResource.create(PK));
		assertEquals(
				res.getListProperty(),
				Assertion.createObjectPropertyAssertion(
						URI.create(OWLClassC.getRefListField()
								.getAnnotation(OWLObjectProperty.class).iri()),
						refList.isInferred()));
		assertEquals(res.getNextNode(), Assertion.createObjectPropertyAssertion(refList
				.getOWLObjectPropertyHasNextIRI().toURI(), refList.isInferred()));
		assertEquals(res.getNodeContent(), Assertion.createObjectPropertyAssertion(refList
				.getOWLPropertyHasContentsIRI().toURI(), refList.isInferred()));
		assertEquals(list.size(), res.getValues().size());
		for (OWLClassA a : list) {
			assertTrue(res.getValues().contains(NamedResource.create(a.getUri())));
		}
	}

	@Test
	public void extractsValuesIntoAxiomsForSaveFromEmptyList() throws Exception {
		final OWLClassC c = new OWLClassC(PK);
		c.setReferencedList(Collections.<OWLClassA> emptyList());
		final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK),
				descriptor.getContext());
		strategy.buildAxiomValuesFromInstance(c, builder);

		final List<ReferencedListValueDescriptor> descriptors = OOMTestUtils
				.getReferencedListValueDescriptors(builder);
		assertEquals(1, descriptors.size());
		final ReferencedListValueDescriptor res = descriptors.get(0);
		assertTrue(res.getValues().isEmpty());
	}

	@Test
	public void extractsValuesIntoAxiomsForSaveFromNullList() throws Exception {
		final OWLClassC c = new OWLClassC(PK);
		c.setReferencedList(null);
		final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK),
				descriptor.getContext());
		strategy.buildAxiomValuesFromInstance(c, builder);

		final List<ReferencedListValueDescriptor> descriptors = OOMTestUtils
				.getReferencedListValueDescriptors(builder);
		assertEquals(1, descriptors.size());
		final ReferencedListValueDescriptor res = descriptors.get(0);
		assertTrue(res.getValues().isEmpty());
	}
}
