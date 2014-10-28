package cz.cvut.kbss.jopa.oom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.utils.TestEnvironmentUtils;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.AxiomImpl;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

public class SimpleListPropertyStrategyTest {

	private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityC");

	@Mock
	private EntityType<OWLClassC> etC;

	@Mock
	private ListAttribute simpleList;

	@Mock
	private ListAttribute refList;

	@Mock
	private Identifier idC;

	@Mock
	private EntityMappingHelper mapperMock;

	private Descriptor descriptor;

	private SimpleListPropertyStrategy strategy;

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		TestEnvironmentUtils.initOWLClassCMocks(etC, simpleList, refList, idC);

		this.descriptor = new EntityDescriptor();
		this.strategy = new SimpleListPropertyStrategy(etC, simpleList, descriptor, mapperMock);
	}

	@Test
	public void addsValueFromAxiomAndVerifiesCorrectDescriptorWasCreated() {
		final Axiom<URI> ax = new AxiomImpl<>(NamedResource.create(PK),
				Assertion.createObjectPropertyAssertion(simpleList.getIRI().toURI(), false),
				new Value<>(URI.create("http://someSequence.org")));
		final Collection<Axiom<?>> axioms = Collections.emptyList();
		when(mapperMock.loadSimpleList(any(SimpleListDescriptor.class))).thenReturn(axioms);

		strategy.addValueFromAxiom(ax);
		final ArgumentCaptor<SimpleListDescriptor> captor = ArgumentCaptor
				.forClass(SimpleListDescriptor.class);
		verify(mapperMock).loadSimpleList(captor.capture());
		final SimpleListDescriptor res = captor.getValue();
		assertEquals(PK, res.getListOwner().getIdentifier());
		assertEquals(simpleList.getIRI().toURI(), res.getListProperty().getIdentifier());
		assertEquals(simpleList.getOWLObjectPropertyHasNextIRI().toURI(), res.getNextNode()
				.getIdentifier());
		assertNull(res.getContext());
	}

	@Test
	public void buildsInstanceFieldFromAxioms() throws Exception {
		final Axiom<URI> ax = new AxiomImpl<>(NamedResource.create(PK),
				Assertion.createObjectPropertyAssertion(simpleList.getIRI().toURI(), false),
				new Value<>(URI.create("http://someSequence.org")));
		final Collection<Axiom<?>> axioms = new ArrayList<>();
		final List<OWLClassA> entitiesA = new ArrayList<>();
		URI previous = PK;
		for (int i = 0; i < 5; i++) {
			final URI uri = URI.create("http://entity" + i);
			final Axiom<URI> a = new AxiomImpl<>(NamedResource.create(previous),
					Assertion.createObjectPropertyAssertion(simpleList
							.getOWLObjectPropertyHasNextIRI().toURI(), false), new Value<>(uri));
			axioms.add(a);
			final OWLClassA entityA = new OWLClassA();
			entityA.setUri(uri);
			entitiesA.add(entityA);
			when(mapperMock.getEntityFromCacheOrOntology(OWLClassA.class, uri, descriptor))
					.thenReturn(entityA);
			previous = uri;
		}
		when(mapperMock.loadSimpleList(any(SimpleListDescriptor.class))).thenReturn(axioms);

		strategy.addValueFromAxiom(ax);
		final OWLClassC instance = new OWLClassC();
		instance.setUri(PK);
		strategy.buildInstanceFieldValue(instance);
		assertEquals(entitiesA.size(), instance.getSimpleList().size());
		for (OWLClassA a : entitiesA) {
			assertTrue(instance.getSimpleList().contains(a));
		}
	}

}
