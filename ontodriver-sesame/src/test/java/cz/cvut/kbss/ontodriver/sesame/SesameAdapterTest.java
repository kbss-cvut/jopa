package cz.cvut.kbss.ontodriver.sesame;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.repository.Repository;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.sail.memory.MemoryStore;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver_new.MutationAxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.OntoDriverProperties;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

public class SesameAdapterTest {

	private static final String LANGUAGE = "en";
	private static final NamedResource SUBJECT = NamedResource
			.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityX");

	private static ValueFactory vf;
	private static Repository repo;

	@Mock
	private Connector connectorMock;

	private SesameAdapter adapter;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		final MemoryStore mStore = new MemoryStore();
		repo = new SailRepository(mStore);
		vf = repo.getValueFactory();
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(connectorMock.getValueFactory()).thenReturn(vf);
		this.adapter = new SesameAdapter(connectorMock, Collections.<String, String> singletonMap(
				OntoDriverProperties.ONTOLOGY_LANGUAGE, LANGUAGE));

	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		repo.shutDown();
	}

	@Test
	public void testSesameAdapter() throws Exception {
		this.adapter = new SesameAdapter(connectorMock, Collections.<String, String> emptyMap());
		final Field langField = SesameAdapter.class.getDeclaredField("language");
		langField.setAccessible(true);
		final String lang = (String) langField.get(adapter);
		assertNull(lang);
	}

	@Test
	public void testGetContexts() throws Exception {
		final List<Resource> contexts = new ArrayList<>();
		for (int i = 0; i < 5; i++) {
			contexts.add(vf.createURI("http://krizik.felk.cvut.cz/ontologies/context" + i));
		}
		when(connectorMock.getContexts()).thenReturn(contexts);
		final List<URI> res = adapter.getContexts();
		assertEquals(contexts.size(), res.size());
		for (int i = 0; i < contexts.size(); i++) {
			assertEquals(contexts.get(i).stringValue(), res.get(i).toString());
		}
	}

	@Test
	public void testGetContextsWithBNodes() throws Exception {
		final List<Resource> contexts = new ArrayList<>();
		int bnodes = 0;
		for (int i = 0; i < 5; i++) {
			if (i % 2 == 1) {
				contexts.add(vf.createBNode());
				bnodes++;
			} else {
				contexts.add(vf.createURI("http://krizik.felk.cvut.cz/ontologies/context" + i));
			}
		}
		when(connectorMock.getContexts()).thenReturn(contexts);
		final List<URI> res = adapter.getContexts();
		assertEquals(contexts.size() - bnodes, res.size());
	}

	@Test
	public void testClose() throws Exception {
		assertTrue(adapter.isOpen());
		adapter.close();
		verify(connectorMock).close();
		assertFalse(adapter.isOpen());
	}

	@Test
	public void testPersistDataPropertiesNoContext() throws Exception {
		final MutationAxiomDescriptor ad = new MutationAxiomDescriptor(SUBJECT);
		ad.addAssertionValue(
				Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
		ad.addAssertionValue(Assertion.createDataPropertyAssertion(URI
				.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute"),
				false), new Value<String>("StringValue"));
		adapter.persist(ad);
		final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
		verify(connectorMock).addStatements(captor.capture());
		final Collection<?> res = captor.getValue();
		assertEquals(2, res.size());
		assertTrue(statementsCorrespondToAxiomDescriptor(ad, (Collection<Statement>) res));
	}

	private boolean statementsCorrespondToAxiomDescriptor(MutationAxiomDescriptor ad,
			Collection<Statement> statements) {
		for (Assertion as : ad.getAssertions()) {
			final String strIdentifier = as.getIdentifier().toString();
			for (Value<?> val : ad.getAssertionValues(as)) {
				boolean valueFound = false;
				for (Statement stmt : statements) {
					if (stmt.getPredicate().stringValue().equals(strIdentifier)
							&& stmt.getObject().stringValue().equals(val.getValue().toString())) {
						valueFound = true;
						assertEquals(ad.getSubject().getIdentifier().toString(), stmt.getSubject()
								.stringValue());
						if (ad.getAssertionContext(as) == null) {
							assertNull(stmt.getContext());
						} else if (stmt.getContext() == null) {
							assertNull(ad.getAssertionContext(as));
						} else {
							assertEquals(ad.getAssertionContext(as).toString(), stmt.getContext()
									.stringValue());
						}
						break;
					}
				}
				if (!valueFound) {
					return false;
				}
			}
		}
		return true;
	}

	@Test
	public void testPersistEntityWithTypesDataPropertyInContext() throws Exception {
		final MutationAxiomDescriptor ad = new MutationAxiomDescriptor(SUBJECT);
		ad.addAssertionValue(
				Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
		ad.addAssertionValue(
				Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassC")));
		ad.addAssertionValue(
				Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassD")));
		final Assertion dataAssertion = Assertion.createDataPropertyAssertion(URI
				.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute"),
				false);
		ad.addAssertionValue(dataAssertion, new Value<String>("StringValue"));
		ad.setAssertionContext(dataAssertion,
				URI.create("http://krizik.felk.cvut.cz/ontologies/contextOne"));
		adapter.persist(ad);
		final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
		verify(connectorMock).addStatements(captor.capture());
		final Collection<?> res = captor.getValue();
		assertEquals(4, res.size());
		assertTrue(statementsCorrespondToAxiomDescriptor(ad, (Collection<Statement>) res));
	}

	@Test
	public void testPersistEntityWithObjectPropertyMultipleValues() throws Exception {
		final MutationAxiomDescriptor ad = new MutationAxiomDescriptor(SUBJECT);
		ad.addAssertionValue(
				Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
		final Assertion objectAssertion = Assertion.createObjectPropertyAssertion(
				URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-owlclassY"),
				false);
		ad.addAssertionValue(objectAssertion,
				new Value<URI>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityY")));
		ad.addAssertionValue(objectAssertion,
				new Value<URI>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityYY")));
		ad.addAssertionValue(objectAssertion,
				new Value<URI>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityYYY")));
		ad.setSubjectContext(URI.create("http://krizik.felk.cvut.cz/ontologies/contextOne"));
		adapter.persist(ad);
		final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
		verify(connectorMock).addStatements(captor.capture());
		final Collection<?> res = captor.getValue();
		assertEquals(4, res.size());
		assertTrue(statementsCorrespondToAxiomDescriptor(ad, (Collection<Statement>) res));
	}

	@Test
	public void testPersistEntityWithProperties() throws Exception {
		final MutationAxiomDescriptor ad = new MutationAxiomDescriptor(SUBJECT);
		ad.addAssertionValue(
				Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
		final Assertion objectAssertion = Assertion.createPropertyAssertion(
				URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-owlclassY"),
				false);
		ad.addAssertionValue(objectAssertion,
				new Value<URI>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityY")));
		final Assertion dataAssertion = Assertion.createPropertyAssertion(URI
				.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute"),
				false);
		ad.addAssertionValue(dataAssertion, new Value<String>("stringValue"));
		adapter.persist(ad);
		final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
		verify(connectorMock).addStatements(captor.capture());
		final Collection<?> res = captor.getValue();
		assertEquals(3, res.size());
		assertTrue(statementsCorrespondToAxiomDescriptor(ad, (Collection<Statement>) res));
	}

	@Test
	public void testPersistEntityWithAnnotationPropertyAndDifferentSubjectAndPropertyContexts()
			throws Exception {
		final URI subjectCtx = URI.create("http://krizik.felk.cvut.cz/ontologies/contextOne");
		final URI propertyCtx = URI.create("http://krizik.felk.cvut.cz/ontologies/contextTwo");
		final MutationAxiomDescriptor ad = new MutationAxiomDescriptor(SUBJECT);
		ad.setSubjectContext(subjectCtx);
		ad.addAssertionValue(
				Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
		final Assertion dataAssertion = Assertion.createAnnotationPropertyAssertion(
				URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-annotation"),
				false);
		ad.addAssertionValue(dataAssertion, new Value<String>("StringValue"));
		ad.setAssertionContext(dataAssertion, propertyCtx);
		adapter.persist(ad);
		final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
		verify(connectorMock).addStatements(captor.capture());
		final Collection<?> res = captor.getValue();
		assertEquals(2, res.size());
		assertTrue(statementsCorrespondToAxiomDescriptor(ad, (Collection<Statement>) res));
		// This checks that the contexts are set correctly
		for (Object s : res) {
			final Statement stmt = (Statement) s;
			if (stmt.getPredicate().stringValue().equals(dataAssertion.getIdentifier().toString())) {
				assertEquals(propertyCtx.toString(), stmt.getContext().stringValue());
			} else {
				assertEquals(subjectCtx.toString(), stmt.getContext().stringValue());
			}
		}
	}
}
