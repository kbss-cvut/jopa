package cz.cvut.kbss.jopa.oom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import cz.cvut.kbss.jopa.model.annotations.Inferred;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

public class SimpleSetPropertyStrategyTest {

	private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityC");

	@Mock
	private EntityType<OWLClassJ> etJ;

	@Mock
	private PluralAttribute simpleSet;

	@Mock
	private Identifier idJ;

	@Mock
	private EntityType<OWLClassA> etA;

	@Mock
	private Identifier idA;

	@Mock
	private EntityMappingHelper mapperMock;

	@Mock
	private CascadeResolver cascadeResolverMock;

	private Descriptor descriptor;

	private SimpleSetPropertyStrategy<OWLClassJ> strategy;

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		TestEnvironmentUtils.initOWLClassJMocks(etJ, simpleSet, idJ);
		when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(etA);
		when(etA.getIdentifier()).thenReturn(idA);
		when(idA.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));

		this.descriptor = new EntityDescriptor();
		this.strategy = new SimpleSetPropertyStrategy<>(etJ, simpleSet, descriptor, mapperMock);
		strategy.setCascadeResolver(cascadeResolverMock);
	}

	@Test
	public void extractsValuesFromInstance() throws Exception {
		final OWLClassJ j = new OWLClassJ();
		j.setUri(PK);
		j.setOwlClassA(generateSet());
		final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
		strategy.buildAxiomValuesFromInstance(j, builder);
		final AxiomValueDescriptor res = OOMTestUtils.getAxiomValueDescriptor(builder);
		assertEquals(NamedResource.create(PK), res.getSubject());
		final OWLObjectProperty op = OWLClassJ.getOwlClassAField().getAnnotation(
				OWLObjectProperty.class);
		final Assertion ass = Assertion.createObjectPropertyAssertion(URI.create(op.iri()),
				OWLClassJ.getOwlClassAField().getAnnotation(Inferred.class) != null);
		assertEquals(j.getOwlClassA().size(), res.getAssertionValues(ass).size());
		for (OWLClassA aa : j.getOwlClassA()) {
			assertTrue(res.getAssertionValues(ass).contains(new Value<>(NamedResource.create(aa.getUri()))));
		}
		verify(cascadeResolverMock, times(j.getOwlClassA().size())).resolveFieldCascading(
				eq(simpleSet), any(Object.class), eq((URI) null));
	}

	private Set<OWLClassA> generateSet() {
		final Set<OWLClassA> set = new HashSet<>();
		for (int i = 0; i < 10; i++) {
			final OWLClassA a = new OWLClassA();
			a.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA_" + i));
			set.add(a);
		}
		return set;
	}

	@Test
	public void extractsValuesFromInstanceSetIsNull() throws Exception {
		final OWLClassJ j = new OWLClassJ();
		j.setUri(PK);
		j.setOwlClassA(null);
		final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
		strategy.buildAxiomValuesFromInstance(j, builder);
		final AxiomValueDescriptor res = OOMTestUtils.getAxiomValueDescriptor(builder);
		assertEquals(NamedResource.create(PK), res.getSubject());
		final OWLObjectProperty op = OWLClassJ.getOwlClassAField().getAnnotation(
				OWLObjectProperty.class);
		final Assertion ass = Assertion.createObjectPropertyAssertion(URI.create(op.iri()),
				OWLClassJ.getOwlClassAField().getAnnotation(Inferred.class) != null);
		assertEquals(1, res.getAssertionValues(ass).size());
		assertSame(Value.nullValue(), res.getAssertionValues(ass).get(0));
	}

	@Test
	public void throwsExceptionWhenMinimumCardinalityConstraintIsViolated() throws Exception {
		final Set<OWLClassA> set = generateSet();

	}
}
